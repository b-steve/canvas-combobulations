library(httr)
library(jsonlite)

## A function to get data from the Canvas API via a URL.
get.data <- function(url){
    resp.fun.args <- list(url = url,
                          user_agent("hightops"),
                          add_headers(Authorization = paste("Bearer", token)),
                          query = list(per_page = 100, user_id = NULL))
    resp <- do.call("GET", resp.fun.args)
    json <- content(resp, "text")
    fromJSON(json, flatten = FALSE)
}

## A function to determine grades based on a group assessment and peer
## assessments.
grade.fun <- function(group, individual){
    n.students <- length(individual)
    out <- numeric(n.students)
    if (!is.null(names(individual))){
        names(out) <- names(individual)
    }
    for (i in 1:n.students){
        if (individual[i] < 2.5){
            out[i] <- group/2.5 + ((group - group/2.5)/1.5)*(individual[i] - 1)
        } else {
            out[i] <- group + 0.02*group*(individual[i] - 2.5)
        }
    }
    out[out > 100] <- 100
    out
}

## The main function to calculate individual grades.
## assignment.id: The Canvas ID for the main assignment.
## assignment.pa.id: The Canvas ID for the peer-assessment assignment.
## group.grades: A list of group grades, with group names matching those on Canvas.
## grade.fun: A function to determine grades based on a group assessment and peer assessments.
## domain: The Canvas domain for your institution.
## course.id: The Canvas ID for the course.
calc.grades <- function(assignment.id, assignment.pa.id, group.grades, grade.fun, domain = "https://canvas.auckland.ac.nz", course.id = 62489){
    ## Getting student list.
    url <- paste(domain, "/api/v1", "courses", course.id, "users", sep = "/")
    people.df <- get.data(url)
    
    ## Getting assignment information.
    url <- paste(domain, "/api/v1", "courses", course.id, "assignments", assignment.id, sep = "/")
    assign.info <- get.data(url)
    
    ## Getting group category ID.
    group.category.id <- assign.info$group_category_id
    
    ## Getting peer-assessment assignment information.
    url <- paste(domain, "/api/v1", "courses", course.id, "assignments", assignment.pa.id, sep = "/")
    assign.pa.info <- get.data(url)

    ## Getting rubric ID.
    rubric.id <- assign.pa.info$rubric_settings$id

    ## Getting the association ID for the assignment-to-rubric matching.
    url <- paste0(paste(domain, "/api/v1", "courses", course.id, "rubrics", rubric.id, sep = "/"),
                  "?", "include=associations")
    rubric.association.df <- get.data(url)$associations
    rubric.association.id <-
        rubric.association.df$id[rubric.association.df$association_id == assignment.pa.id]
    
    ## Getting peer-review completion summary.
    url <- paste(domain, "/api/v1", "courses", course.id, "assignments", assignment.pa.id,
                 "peer_reviews",  sep = "/")
    pr.df <- get.data(url)

    ## Getting peer-assessment information.
    url <- paste0(paste(domain, "/api/v1", "courses", course.id, "rubrics", rubric.id, sep = "/"),
                  "?", "include=peer_assessments")
    rubric.info <- get.data(url)
    pr.assessments.df <- rubric.info$assessments
    
    ## Keeping only the assessments from the assignment we want.
    pr.assessments.df <-
        pr.assessments.df[pr.assessments.df$rubric_association_id == rubric.association.id, ]

    ## Combining the completion and assessment data frame and adding student/assessor names.
    n.pas <- nrow(pr.df)
    n.completed <- nrow(pr.assessments.df)
    all.names <- people.df$short_name
    names(all.names) <- people.df$id
    student.name <- all.names[as.character(pr.df$user_id)]
    assessor.name <- all.names[as.character(pr.df$assessor_id)]
    score <- rep(NA, n.pas)
    for (i in 1:n.completed){
        score[pr.df$asset_id == pr.assessments.df$artifact_id[i] &
              pr.df$assessor_id == pr.assessments.df$assessor_id[i]] <- pr.assessments.df$score[i]
    }
    pr.df <- data.frame(pr.df, student_name = student.name,
                        assessor_name = assessor.name, score = score)
    pr.df <- pr.df[, c(2, 6:9)]
    
    ## Getting group category information.
    url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
    group.category.df <- get.data(url)
    group.category.df <- group.category.df[group.category.df$group_category_id == group.category.id, ]
    
    ## Getting information for the different groups.
    group.ids <- group.category.df$id
    n.groups <- nrow(group.category.df)
    group.info <- vector(mode = "list", length = n.groups)
    for (i in 1:n.groups){
        url <- paste(domain, "/api/v1", "groups", group.ids[i], "users", sep = "/")
        group.info[[i]] <- get.data(url)
    }
    group.names <- group.category.df$name
    names(group.info) <- group.names
    
    ## Creating a data frame with each individual's information.
    individual.df <- people.df[people.df$id %in% unique(pr.df$user_id), c(1, 2)]
    n.students <- nrow(individual.df)
    individual.df$pa.score <- tapply(pr.df$score, pr.df$user_id,
                                     mean, na.rm = TRUE)[as.character(individual.df$id)]/
        rubric.info$points_possible*5
    individual.df$p.completed <- tapply(pr.df$score, pr.df$assessor_id,
                                        function(x){
                                            mean(!is.na(x))}
                                        )[as.character(individual.df$id)]
    
    ## Putting the group information into the individual data frame.
    group.id <- numeric(n.students)
    group.name <- character(n.students)
    for (i in 1:n.groups){
        n.members <- nrow(group.info[[i]])
        for (j in 1:n.members){
            group.id[individual.df$id == group.info[[i]]$id[j]] <- group.ids[i]
            group.name[individual.df$id == group.info[[i]]$id[j]] <- group.names[i]
        }
    }
    individual.df$group.id <- group.id
    individual.df$group.name <- group.name

    ## Putting the group grades into the individual data frame.
    individual.df$group.grade <- c(group.grades[individual.df$group.name], recursive = TRUE)
    
    ## Calculating final grades.
    final.grade <- numeric(n.students)
    for (i in 1:n.groups){
        final.grade[individual.df$group.name == group.names[i]] <-
            grade.fun(group.grades[[group.names[i]]],
                      individual.df$pa.score[individual.df$group.name == group.names[i]])
    }
    individual.df$final.grade <- final.grade
    list(individual = individual.df, pr = pr.df)
}
