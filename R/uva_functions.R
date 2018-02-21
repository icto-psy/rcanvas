#' Title
#' @import httr
#' @param course.id numeric The Canvas Course ID
#' @param account.id numeric The Canvas Account ID
#' @param course.code character "Vakcode" matches the course code returned by API
#' @param user.id numeric The Canvas User ID
#' @param sis.user.id character The SIS User ID
#'
#' @return Vector with either true or false, should be same length as input.
#' @export
#'
#' @examples
#' checkStudentInCourse(course.id = NULL, course.code = "7203BPBCXY" , user.id = NULL, sis.user.id = NULL, account.id = NULL)
checkStudentInCourse <- function(course.id = NULL, course.code = NULL, user.id = NULL, sis.user.id = NULL, account.id = NULL){
  ## TODO: Issue error if input is not of same length.
  ## TODO: What is a course has the same course.code, but is given in a new year?? (Maybe use sis.course.id and use )

  # If no sis user ID or SIS USER ID is provided. Stop.

  if(is.null(user.id) & is.null(sis.user.id)) stop("Provide at least one student")

  if(is.null(course.id) & is.null(course.code)) stop("Provide at least one course")

  # If account ID is not provided, check top-account and list all contained courses. Issue warning.

  if(is.null(account.id)) warning("If you do not provide an account ID, the API call will retrieve all courses in the root account, possibly making this function slow")

  if (!is.null(account.id)) {
    url <- paste0(canvas_url(), paste("accounts", account.id, "courses", sep = "/"))
  } else {
    res <- httr::GET(paste0(canvas_url(),"accounts"), add_headers(Authorization = paste("Bearer", check_token())))
    account.id <- content(res)[[1]]$id
    url <- paste0(canvas_url(), paste("accounts", account.id, "courses", sep = "/"))
  }
  # Retrieve all courses in either top-account or provided account.
  args <- list(
    per_page = 100
  )
  dat <- process_response(url, args)
  # Check course code or course id against all resulting courses.
  if(!is.null(course.id)) {
    courses_in_dat_frame <- course.id %in% dat$id
  }
  if (!is.null(course.code)) {
    courses_in_dat_frame <- course.code %in% dat$course_code
    course.id <- dat$id[dat$course_code %in% course.code]
  }
  #If one of the courses is missing, error with courses not working.
  ## TODO: Correct this to show vakcode.
  if(any(courses_in_dat_frame == FALSE)){
    stop(paste("The following course/s could not be found:", unique(paste0(course.id[!courses_in_dat_frame]))))
  }
  #Extract course ids to query.

  ids_to_query <- unique(dat$id[dat$id %in% course.id])

  # Extract the students from each course and check against provided student IDs or SiS IDs

  result_list <- list(0)
  for(i in seq_along(ids_to_query)){
    url <- paste0(canvas_url(), paste("courses", ids_to_query[i], "users", sep = "/"))

    args <- list(per_page = 100,
                 enrollment_type = "student")

    dat <- process_response(url, args)

    if(!is.null(user.id)){
      result_list[[i]] <- user.id %in% dat$id
    }
    if(!is.null(sis.user.id)){
      result_list[[i]] <- as.character(sis.user.id) %in% dat$sis_user_id
    }
    return(unlist(result_list))
  }
}
