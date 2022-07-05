attendee <- function(joined, full_name, initials, notes) {
  output <- list(
    full_name = full_name,
    initials= initials,
    joined = joined,
    notes = notes
  )

  class(output) <- c("attendee", class(output))
  output
}



attendee_table <- function(attendees) {
  tibble::tibble(
    name = purrr::map_chr(attendees, "full_name"),
    initials = purrr::map_chr(attendees, "initials") |>
      purrr::map(tolower) |>
      purrr::map(eval_attendee_vector)
    # notes = purrr::map_chr(attendees, "notes")
  ) |>
    gt::gt() |>
    gt::fmt_passthrough(columns = "initials",
                        escape = FALSE)
}



make_attendee <- function(person) {
  paste0('<span title="', person$full_name, '" style="font-weight:bold">', person$initials, '</span>')
}

init_attendees <- function(attendees) {
  add_attendee_to_parent <- function(attendee) {
    assign(tolower(attendee$initials), value = make_attendee(attendee), envir = .GlobalEnv)
  }

  purrr::walk(attendees, add_attendee_to_parent)
}


eval_attendee_vector <- function(inits) {
  purrr::map(inits, rlang::sym) |>
    purrr::map_chr(rlang::eval_bare) |>
    paste(collapse = ", ")
}
