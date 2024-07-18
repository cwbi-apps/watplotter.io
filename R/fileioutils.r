WAT.DATE.ORIGIN = "1899-12-30"

#' File Extension Handling
#'
#' Extract or strip file extensions.
#'
#' @param file A file name or path.
#' 
#' @importFrom stringr str_extract
#' @importFrom dplyr if_else
#' @keywords internal
get_file_extension = function(file) {
  ext = str_extract(file, "\\.[:alnum:]+$")
  if_else(is.na(ext) & !is.na(file), "", ext)
}

#' @rdname get_file_extension
#' @importFrom stringr str_replace
#' @keywords internal
strip_file_extension = function(file) {
  str_replace(file, "\\.[:alnum:]+$", "")
}


#' Path To Binary File Name
#'
#' Convert a path to its standard binary file name.
#'
#' @param path A vector of paths.
#' @inheritParams base::tempfile
#' @inheritParams binary_name_to_path
#' @return A filename
#'
#' @importFrom watplotter.core check_path
#' @importFrom stringr str_replace
#' @keywords internal
path_to_binary_name = function(path, fileext = ".dat", binsep = "__") {
  check = try(check_path(path), silent = TRUE)
  if (inherits(check, "try-error")) {
    warning(attr(check, "condition")$message)
  }
  paste0(str_replace_all(path, "/", binsep), fileext)
}


#' Binary Name to Path
#'
#' Convert a binary file name to a path.
#'
#' @param file The path to the binary file.
#' @param binsep The separator used in WAT binary files to replace the
#'   path separator "/".
#' @return The equivalent path.
#'
#' @importFrom watplotter.core check_path
#' @importFrom stringr str_replace_all
#' @keywords internal
binary_name_to_path = function(file, binsep = "__") {
  path = str_replace_all(strip_file_extension(basename(file)),
    binsep, "/")
  check = try(check_path(path), silent = TRUE)
  if (inherits(check, "try-error")) {
    warning(attr(check, "condition")$message)
  }
  path
}


#' Read WAT Binary File
#'
#' Read a WAT binary file into a dataframe.
#'
#' @param file A vector of file paths, or a list of raw vectors.
#' @param ... Named identifier columns to be
#'   joined to the return data, e.g., the alternative name or other
#'   information. Each argument must match length and order of `file`.
#' @inheritParams binary_name_to_path
#' @inheritParams read_bin_file
#' @param skip_missing If `TRUE`, skip over missing files with a
#'   warning.
#' @return A tibble with fields "path", "date", "event", "value" and
#'   (if supplied) additional fields from `label`.
#'
#' @importFrom dplyr select left_join all_of bind_rows
#' @importFrom purrr map set_names map_lgl
#' @importFrom stringr str_glue
#' @export
read_wat_binary = function(file, ..., tz = "UTC", format = ">i4;>i4;>f4",
  skip_missing = FALSE) {

  # format checking
  format = unique(format)
  if (length(format) != 1L) {
    stop("Argument \"format\" must be length 1.")
  }
  # prepare labels
  id = list(...)
  id_names = names(id)


  if (inherits(file, "character")) {
    file = as.list(file)
    in_class = "character"
  } else if (inherits(file, "raw")) {
    file = list(file)
    in_class = "raw"
  } else if (inherits(file, "list")) {
    if (all(map_lgl(file, function(x) inherits(x, "character")))) {
      in_class = "character"
    } else if (all(map_lgl(file, function(x) inherits(x, "character")))) {
      in_class = "raw"
    } else {
      in_class = class(file[[1]])
    }
  } else {
    in_class = class(file)
  }

  if (!(in_class %in% c("character", "raw"))) {
    stop("Elements of argument \"file\" must be class 'character' ",
      "or 'raw'.")
  }

  if (in_class == "character") {
    # argument checking
    file = unlist(file)
    if (is.null(names(file))) {
      names(file) = file
    }
    checks = file.exists(file)
    msg = paste0("One or more files are missing:\n",
      paste(str_glue("    {which(!checks)}: {names(file)[!checks]}"),
        collapse = "\n"))
  } else {
    if (is.null(names(file))) {
      names(file) = paste0("raw", seq_along(file))
    }
    checks = map_lgl(unname(file), function(x)
        length(x) > 0L)
    msg = paste0("One or more entries are empty:\n",
      paste(str_glue("    {which(!checks)}: {names(file)[!checks]}"),
        collapse = "\n"))
  }
  if (any(!checks)) {
    if (skip_missing) {
      warning(msg)
    } else {
      stop(msg)
    }
  }
  id[["file"]] = names(file)
  labels = as.data.frame(id)
  # get data
  result = bind_rows(map(file[checks], function(x)
      read_bin_file(x, tz = tz, format = format)),
    .id = "file")
  # add labels and return
  keep_names = c(id_names, setdiff(names(result), "file"))
  select(left_join(result, labels, by = "file"), all_of(keep_names))
}


#' Read Binary File
#'
#' Read a binary file into a dataframe.
#'
#' @inheritParams binary_name_to_path
#' @param tz The timezone to use when parsing datetimes.
#' @param format The format string, in form `"(endian)(type)(bytes)"`.
#' fields are separated with `";"`
#' @return A tibble with fields "date", "event", "value".
#'
#' @details The input binary file is expected to match this byte order:
#' - bytes 1-4: integer (date)
#' - bytes 5-8: integer (event)
#' - bytes 9-12: float (value)
#'
#' @importFrom dplyr as_tibble
#' @importFrom purrr map imap map_int
#' @importFrom stringr str_split_1 str_squish
#' @importFrom lubridate as_datetime
#' @importFrom watplotter.core fields
#' @importFrom utils head
#' @keywords internal
read_bin_file = function(file, tz = "UTC", format = ">i4;>i4;>f4") {
  # parse format string
  formats = map(str_split_1(format, ";"), bin_format)
  # guess binary file type based on number of fields
  if (length(formats) == 3L) {
    # time series data
    field_names = fields("date", "event", "value")
  } else if (length(formats) == 2L) {
    # paired data
    field_names = fields("event", "baseyear")
  } else {
    stop("Unknown binary file structure ", shQuote(format), ".")
  }
  record_size = sum(map_int(formats, "size"))
  # get file specs
  if (inherits(file, "character")) {
    raw_vec = readBin(file, what = "raw", n = file.size(file))
  } else if (inherits(file, "raw")) {
    raw_vec = file
  }
  vec_size = length(raw_vec)
  if (vec_size %% record_size > 0L) {
    stop("Size of data does not match specified format.")
  }
  num_records = vec_size %/% record_size
  # read the raw binary
  raw_mat = matrix(raw_vec, nrow = record_size, ncol = num_records,
    byrow = FALSE)
  # loop through format specs and read fields
  offset = cumsum(c(0L, head(map_int(formats, "size"), -1L)))
  col_list = imap(formats, function(fmt, i) {
    readBin(raw_mat[seq_len(fmt$size) + offset[i], ],
      what = fmt$what, size = fmt$size, endian = fmt$endian,
      n = num_records)
  })
  names(col_list) = field_names
  if (fields("date") %in% names(col_list)) {
    wat_origin = as_datetime(WAT.DATE.ORIGIN, tz = tz)
    col_list[[fields("date")]] = as_datetime(col_list[[fields("date")]] * 60,
      tz = tz, origin = wat_origin)
  }
  if (fields("baseyear") %in% names(col_list)) {
    col_list[[fields("baseyear")]] = factor(col_list[[fields("baseyear")]],
      levels = unique(col_list[[fields("baseyear")]]),
      labels = str_squish(unique(col_list[[fields("baseyear")]])))
  }
  as_tibble(col_list)
}


#' Parse Binary Format String
#'
#' Parse the binary file format string for use with [readBin()].
#'
#' @param format The format string.
#' @return A list of format components.
#'
#' @importFrom stringr str_extract
#' @importFrom dplyr case_when
#' @keywords internal
bin_format = function(format) {
  fmt = str_extract(format, "^([<>])([:alpha:])([0-9]+)$",
    group = c(1L, 2L, 3L))
  endian = switch(fmt[1],
    ">" = "big",
    "<" = "little",
    NA_character_
  )
  what = switch(fmt[2],
    "i" = "integer",
    "f" = "double",
    "s" = "character",
    NA_character_
  )
  size = as.integer(fmt[3])
  out = list(what = what, size = size, endian = endian)
  if (anyNA(out)) {
    stop("Did not recognize format string component ",
      shQuote(format), ".", call. = FALSE)
  }
  out
}


# Write WAT Binary Files
#'
#' Write a dataframe to one or binary files, as determined by the
#'   dataframe's `"path"` field.
#'
#' @param df A dataframe. Must include field "path".
#' @param output_dir The directory to write the binary files to.
#' @param validate_paths If `TRUE`, validate the format of all paths
#'   before writing.
#' @return (invisibly) A vector of binary file paths.
#'
#' @importFrom watplotter.core check_path assert_fields
#' @importFrom dplyr filter
#' @importFrom purrr map2_chr
#' @importFrom rlang .data .env
#' @export
write_wat_binary = function(df, output_dir = ".", validate_paths = FALSE) {
  assert_fields(df, "path")
  paths = unique(df[[fields("path")]])
  if (validate_paths) {
    check_path(paths)
  }
  bnames = path_to_binary_name(paths)
  invisible(map2_chr(paths, bnames, function(x, y)
    write_bin_file(filter(df, .data[[fields("path")]] == .env$x),
      file.path(output_dir, y))))
}


#' Write Binary File
#'
#' Write a dataframe to a WAT binary file.
#'
#' @param df A dataframe. Must include fields "date", "event",
#'   and "value".
#' @param file The file path to write to.
#' @return file, invisibly.
#'
#' @details The output binary file is structured as 12-byte sequences:
#' - bytes 1-4: integer (date)
#' - bytes 5-8: integer (event)
#' - bytes 9-12: float (value)
#'
#' @importFrom lubridate tz
#' @importFrom stringr str_extract str_pad
#' @keywords internal
write_bin_file = function(df, file) {
  if (all(fields("date", "event", "value") %in% names(df))) {
    # first write to raw to enforce number of bits
    wat_origin_num = as.numeric(as_datetime(WAT.DATE.ORIGIN,
      tz = tz(df[[fields("date")]]))) / 60
    date_nums = as.integer(as.numeric(df[[fields("date")]]) / 60 - wat_origin_num)
    date_bits = writeBin(date_nums, raw(), size = 4L, endian = "big")
    event_bits = writeBin(as.integer(df[[fields("event")]]), raw(), size = 4L,
      endian = "big")
    value_bits = writeBin(df[[fields("value")]], raw(), size = 4L,
      endian = "big")
    # Turn into an 12xN matrix
    # each column is a (integer, integer, float) record
    raw = rbind(
      matrix(date_bits, nrow = 4L, byrow = FALSE),
      matrix(event_bits, nrow = 4L, byrow = FALSE),
      matrix(value_bits, nrow = 4L, byrow = FALSE)
    )
  } else if (all(fields("event", "baseyear") %in% names(df))) {
    event_bits = writeBin(as.integer(df[[fields("event")]]), raw(), size = 4L,
      endian = "big")
    year_labs = as.character(df[[fields("baseyear")]])
    year_nums = suppressWarnings(as.integer(year_labs))
    if (anyNA(year_nums)) {
      # must be n * 4 - 1 bytes
      n_bytes = 16L
      stopifnot((n_bytes) %% 4L == 0L)
      # baseyear is labels, write characters
      year_labs = str_pad(year_labs, n_bytes - 1L, "right")
      year_bits = writeBin(year_labs, raw(), size = n_bytes,
        endian = "big")
      # Turn into a MxN matrix
      # each column is a (integer, character) record
      # character has extra byte (termination byte?)
      raw = rbind(
        matrix(event_bits, nrow = 4L, byrow = FALSE),
        matrix(year_bits, nrow = n_bytes, byrow = FALSE)
      )
    } else {
      # baseyear is numbers, write integers
      year_bits = writeBin(as.integer(year_nums), raw(), size = 4L,
        endian = "big")
      # Turn into an 8xN matrix
      # each column is a (integer, integer) record
      raw = rbind(
        matrix(event_bits, nrow = 4L, byrow = FALSE),
        matrix(year_bits, nrow = 4L, byrow = FALSE)
      )
    }
  } else {
    stop("Did not recognize data structure (",
      paste(shQuote(names(df)), collapse = " ,"), ").", call. = FALSE)
  }
  # use c() to force to vector
  writeBin(c(raw), file, endian = "big", useBytes = FALSE)
  invisible(file)
}
