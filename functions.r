library(httr)
library(jsonlite)

## A function to get data from the Canvas API via a URL.
get.data <- function(url){
    resp.fun.args <- list(url = url,
                          user_agent("hightops"),
                          add_headers(Authorization = paste("Bearer", token)),
                          query = list(per_page = 100, user_id = NULL))
    resp <- do.call("GET", resp.fun.args)
    header <- headers(resp)$link
    json <- content(resp, "text")
    out <- fromJSON(json, flatten = FALSE)
    if (!is.null(header)){
        header.split <- strsplit(strsplit(header, "<")[[1]], ">")
        header.split <- header.split[2:length(header.split)]
        header.links <- sapply(header.split, function(x) x[1])
        header.link.type <- sapply(strsplit(sapply(header.split, function(x) x[2]), "\""), function(x) x[2])
        if (any(header.link.type == "next")){
            out <- rbind(out, get.data(header.links[header.link.type == "next"]))
        }
    }
    out
}

## A function to create groups.
create.groups <- function(group.names, group.category.name, course.id, domain){
    ## Getting the group category ID number.
    url <- paste(domain, "/api/v1", "courses", course.id, "group_categories", sep = "/")
    group.category.df <- get.data(url)
    group.category.id <- group.category.df$id[group.category.df$name == group.category.name]
    ## Creating the group.
    url <- paste(domain, "/api/v1", "group_categories", group.category.id, "groups", sep = "/")
    description.arg <- "\'description=\'"
    public.arg <- "\'is_public=false\'"
    join.arg <- "\'join_level=parent_context_auto_join\'"
    auth.arg <- paste0("\'Authorization: Bearer ", token, "\'")
    for (i in group.names){
        name.arg <- paste0("\'name=", i, "\'")
        cmd <- paste("curl", url, "-F", name.arg, "-F", public.arg, "-F", join.arg, "-H", auth.arg)
        system(cmd)
    }
}

## A function to add a member to a group.
add.member <- function(group.id, user.id, domain){
    url <- paste(domain, "/api/v1", "groups", group.id, "memberships", sep = "/")
    user.arg <- paste0("\'user_id=", user.id, "\'")
    auth.arg <- paste0("\'Authorization: Bearer ", token, "\'")
    cmd <- paste("curl", url, "-F", user.arg, "-H", auth.arg)
    system(cmd)
}

post.grade <- function(grade, assignment.id, user.id, course.id, domain){
    url <- paste(domain, "/api/v1", "courses", course.id, "assignments",
                 assignment.id, "submissions", "update_grades", sep = "/")
    auth.arg <- paste0("\'Authorization: Bearer ", token, "\'")
    post.arg <- paste0("\'grade_data[", user.id, "][posted_grade]=", grade, "%\'")
    cmd <- paste("curl", url, "-X POST", "-F", post.arg, "-H", auth.arg)
    system(cmd)
}

## The main function to calculate individual grades.
## assignment.id: The Canvas ID for the main assignment.
## assignment.pa.id: The Canvas ID for the peer-assessment assignment.
## group.grades: A list of group grades, with group names matching those on Canvas.
## grade.fun: A function to determine grades based on a group assessment and peer assessments.
## domain: The Canvas domain for your institution.
## course.id: The Canvas ID for the course.
calc.grades <- function(assignment.id, assignment.pa.id, group.grades = NULL, grade.fun, domain = "https://canvas.auckland.ac.nz", course.id){
    ## Indicator for whether we only do peer-assessment participation grading.
    pa.only <- is.null(group.grades)
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
                  "?", "include=peer_assessments&style=full")
    rubric.info <- get.data(url)
    pr.assessments.df <- rubric.info$assessments
    ## Keeping only the assessments from the assignment we want.
    pr.assessments.df <-
        pr.assessments.df[pr.assessments.df$rubric_association_id == rubric.association.id, ]
    ## Separating out the data frame from the data list.
    pr.assessments.data <- pr.assessments.df$data
    pr.assessments.df <- pr.assessments.df[, names(pr.assessments.df) != "data" &
                                             names(pr.assessments.df) != "rubric_association"]
    pr.items.list <- sapply(pr.assessments.data, function(x) x$points, simplify = FALSE)
    n.items <- max(sapply(pr.items.list, length))
    for (i in which(sapply(pr.items.list, length) == 0)){
        pr.items.list[[i]] <- rep(NA, n.items)
    }
    pr.items.df <- do.call(rbind, pr.items.list)
    ## Combining the completion and assessment data frame and adding student/assessor names.
    n.pas <- nrow(pr.df)
    n.completed <- nrow(pr.assessments.df)
    all.names <- people.df$short_name
    names(all.names) <- people.df$id
    student.name <- all.names[as.character(pr.df$user_id)]
    assessor.name <- all.names[as.character(pr.df$assessor_id)]
    score <- rep(NA, n.pas)
    item.scores <- matrix(NA, nrow = n.pas, ncol = n.items)
    for (i in 1:n.completed){
        score[pr.df$asset_id == pr.assessments.df$artifact_id[i] &
              pr.df$assessor_id == pr.assessments.df$assessor_id[i]] <- pr.assessments.df$score[i]
        item.scores[pr.df$asset_id == pr.assessments.df$artifact_id[i] &
              pr.df$assessor_id == pr.assessments.df$assessor_id[i], ] <- pr.items.df[i, ]
    }
    pr.df <- data.frame(pr.df, student_name = student.name,
                        assessor_name = assessor.name)
    pr.df <- pr.df[, c(4, 6:8)]
    colnames(item.scores) <- paste0("item", 1:n.items)
    score <- apply(item.scores, 1, mean, na.rm = TRUE)*n.items
    ## Determining completion of open-ended comments.
    o.completed <- logical(nrow(pr.df))
    for (i in unique(pr.df$user_id)){
        url <- paste0(paste("https://canvas.auckland.ac.nz", "/api/v1", "courses", course.id,
                    "assignments", assignment.pa.id, "submissions", i, sep = "/"), "?",
                    "include=submission_comments")
        comment.df <- get.data(url)$submission_comments
        author.ids <- comment.df$author_id[nchar(comment.df$comment) >= 20]
        o.completed[pr.df$user_id == i & pr.df$assessor_id %in% author.ids] <- TRUE
    }
    pr.df <- data.frame(pr.df, o.completed, score, item.scores) 
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
    individual.df$p.completed <- tapply(apply(item.scores, 1, function(x) mean(!is.na(x))),
                                        pr.df$assessor_id, mean)[as.character(individual.df$id)]
    individual.df$o.completed <- numeric(n.students)
    for (i in 1:n.students){
        individual.df$o.completed[i] <- mean(pr.df$o.completed[pr.df$assessor_id == individual.df$id[i]])
    }
    individual.df$participation.score <- 0.75*individual.df$p.completed + 0.25*individual.df$o.completed
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

    ## Putting stream information into the individual data frame.
    url <- paste(domain, "/api/v1", "courses", course.id, "group_categories", sep = "/")
    stream.category.df <- get.data(url)
    stream.category.id <- stream.category.df$id[stream.category.df$name == "Stream"]
    url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
    stream.df <- get.data(url)
    stream.df <- stream.df[stream.df$group_category_id == stream.category.id, ]
    stream.ids <- stream.df$id
    n.streams <- nrow(stream.df)
    stream.info <- vector(mode = "list", length = n.streams)
    for (i in 1:n.streams){
        url <- paste(domain, "/api/v1", "groups", stream.ids[i], "users", sep = "/")
        stream.info[[i]] <- get.data(url)
    }
    stream.names <- stream.df$name
    stream.id <- numeric(n.students)
    stream.name <- character(n.students)
    for (i in 1:n.streams){
        n.members <- nrow(stream.info[[i]])
        for (j in 1:n.members){
            stream.id[individual.df$id == stream.info[[i]]$id[j]] <- stream.ids[i]
            stream.name[individual.df$id == stream.info[[i]]$id[j]] <- stream.names[i]
        }
    }
    individual.df$stream.id <- stream.id
    individual.df$stream.name <- stream.name
    ## Replacing NaN with NA in the peer-appraisal scores.
    individual.df$pa.score[is.nan(individual.df$pa.score)] <- NA
    final.grade <- numeric(n.students)
    if (pa.only){
        individual.df$group.grade <- numeric(n.students)
    } else {
        ## Putting the group grades into the individual data frame.
        individual.df$group.grade <- c(group.grades[individual.df$group.name], recursive = TRUE)
        ## Calculating final grades.
        for (i in 1:n.groups){
            final.grade[individual.df$group.name == group.names[i]] <-
                grade.fun(group.grades[[group.names[i]]],
                          individual.df$pa.score[individual.df$group.name == group.names[i]])
        }
    }
    individual.df$final.grade <- final.grade
    print(individual.df[, c("name", "participation.score", "group.grade", "pa.score", "final.grade")])
    post <- readline(prompt = "Post grades? Type 'yes' to confirm.")
    if (post == "yes"){
        for (i in 1:n.students){
            if (!pa.only){
                post.grade(round(individual.df$final.grade[i], 1), assignment.id, individual.df$id[i], course.id, domain)
            }
            post.grade(round(100*individual.df$participation.score[i], 1), assignment.pa.id, individual.df$id[i], course.id, domain)
        }
    }
    
    list(individual = individual.df, pr = pr.df)
}

## A function to randomly allocate students to groups.
allocate.groups <- function(group.size, group.category.name, stream, course.id, domain = "https://canvas.auckland.ac.nz"){
    url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
    streams.df <- get.data(url)
    if (stream == "tuesday"){
        stream.id <- streams.df$id[streams.df$name == "STATS/DATASCI 399 - Tuesday Stream"]
    } else if (stream == "friday"){
        stream.id <- streams.df$id[streams.df$name == "STATS/DATASCI 399 - Friday Stream"]   
    } else if (stream == "online"){
        stream.id <- streams.df$id[streams.df$name == "STATS/DATASCI 399 - Online"]
    } else if (stream == "swu"){
        stream.id <- streams.df$id[streams.df$name == "DATASCI 399 - Online SWU"]
    }
    ## Getting student information for this stream.
    url <- paste(domain, "/api/v1", "groups", stream.id, "users", sep = "/")
    student.df <- get.data(url)
    n.students <- nrow(student.df)
    student.names <- student.df$name
    student.ids <- student.df$id
    ## Getting the group category ID number.
    url <- paste(domain, "/api/v1", "courses", course.id, "group_categories", sep = "/")
    group.category.df <- get.data(url)
    group.category.id <- group.category.df$id[group.category.df$name == group.category.name]
    ## Getting the groups only associated with the right activity.
    url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
    groups.df <- get.data(url)
    groups.df <- groups.df[groups.df$group_category_id == group.category.id, ]
    if (nrow(groups.df) == 0){
        start.letter <- "A"
    } else {
        start.letter <- LETTERS[!(LETTERS %in% substr(groups.df$name, nchar(groups.df$name), nchar(groups.df$name)))][1]
    }
    ## Allocating students to groups.
    n.students <- length(student.names)
    n.groups <- floor(n.students/group.size)
    max.size <- ceiling(n.students/n.groups)
    shuffle.order <- sample(n.students)
    shuffled.ids <- c(student.ids[shuffle.order], rep(NA, n.groups*max.size - n.students))
    shuffled.names <- c(student.names[shuffle.order], rep(NA, n.groups*max.size - n.students))
    shuffled.ids.mat <- matrix(shuffled.ids, nrow = n.groups)
    shuffled.names.mat <- matrix(shuffled.names, nrow = n.groups)
    out <- vector(mode = "list", length = n.groups)
    for (i in 1:n.groups) out[[i]] <- shuffled.names.mat[i, ][!is.na(shuffled.names.mat[i, ])]
    start.letter.number <- which(LETTERS == start.letter)
    names(out) <- paste("Group", LETTERS[start.letter.number:(start.letter.number + n.groups - 1)])
    print(out)
    post <- readline(prompt = "Post groups? Type 'yes' to confirm.")
    if (post == "yes"){
        create.groups(names(out), group.category.name, course.id, domain)
        url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
        groups.df <- get.data(url)
        groups.df <- groups.df[groups.df$group_category_id == group.category.id, ]
        for (i in 1:nrow(shuffled.ids.mat)){
            group.id <- groups.df$id[substr(groups.df$name, nchar(groups.df$name), nchar(groups.df$name)) ==
                                     substr(names(out), nchar(names(out)), nchar(names(out)))[i]]
            for (j in 1:ncol(shuffled.ids.mat)){
                user.id <- shuffled.ids.mat[i, j]
                if (!is.na(user.id)){
                    add.member(group.id, user.id, domain)
                }
            }
        }
    }
}

## A function to randomly allocate students to grade other groups.
allocate.graders <- function(n.grades, group.category.name, stream, course.id, domain = "https://canvas.auckland.ac.nz"){
    ## Getting student information for the stream.
    url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
    streams.df <- get.data(url)
    if (stream == "tuesday"){
        stream.id <- streams.df$id[streams.df$name == "STATS/DATASCI 399 - Tuesday Stream"]
    } else if (stream == "friday"){
        stream.id <- streams.df$id[streams.df$name == "STATS/DATASCI 399 - Friday Stream"]   
    } else if (stream == "online"){
        stream.id <- streams.df$id[streams.df$name == "STATS/DATASCI 399 - Online"]
    } else if (stream == "swu"){
        stream.id <- streams.df$id[streams.df$name == "DATASCI 399 - Online SWU"]
    }
    url <- paste(domain, "/api/v1", "groups", stream.id, "users", sep = "/")
    student.df <- get.data(url)[, c("sis_user_id", "id", "name")]
    n.students <- nrow(student.df)
    ## Getting the group category ID number.
    url <- paste(domain, "/api/v1", "courses", course.id, "group_categories", sep = "/")
    group.category.df <- get.data(url)
    group.category.id <- group.category.df$id[group.category.df$name == group.category.name]
    ## Getting the groups only associated with the right activity.
    url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
    groups.df <- get.data(url)
    groups.df <- groups.df[groups.df$group_category_id == group.category.id, ]
    ## Adding group membership to student data frame.
    student.df$group.name <- rep(NA, n.students)
    student.df$group.id <- rep(NA, n.students)
    group.ids <- groups.df$id
    for (i in group.ids){
        ## Getting users for a group.
        url <- paste(domain, "/api/v1", "groups", i, "users", sep = "/")
        user.df <- get.data(url)
        student.df$group.id[student.df$id %in% user.df$id] <- i
        student.df$group.name[student.df$id %in% user.df$id] <- groups.df$name[groups.df$id == i]
    }
    ## Removing groups we don't need for this stream from the groups data frame.
    groups.df <- groups.df[groups.df$id %in% student.df$group.id, ]
    ## Total number of groups.
    n.groups <- nrow(groups.df)
    ## Total number of allocations of graders to make.
    n.total.grades <- n.grades*n.students
    ## Minimum number of grades per group.
    n.grades.per.group <- floor(n.total.grades/n.groups)
    ## Number of graders in each group who need to grade each other group.
    n.grades.per.group.per.group <- floor(n.total.grades/n.groups/(n.groups - 1))
    ## Some empty columns for groups to grade.
    student.df$groups.grade <- rep("", n.students)
    ## First pass: spreading graders across all the other groups.
    for (i in sample(groups.df$name)){
        ## A temporary data frame with students not in group i and with space left to allocate.
        temp.student.df <- student.df[student.df$group.name != i & nchar(student.df$groups.grade) < 2, ]
        ## More than one allocation than groups available, so allocate
        ## a grader from each other group.
        if (n.grades.per.group.per.group > 0){
            for (j in groups.df$name[groups.df$name != i]){
                ## Getting candidate IDs. These belong to group j and
                ## have the smallest number of previous allocations.
                sub.temp.student.df <- temp.student.df[temp.student.df$group.name == j, ]
                if (sum(nchar(sub.temp.student.df$groups.grade) == min(nchar(sub.temp.student.df$groups.grade))) >= n.grades.per.group.per.group){
                    candidate.ids <- sub.temp.student.df[nchar(sub.temp.student.df$groups.grade) == min(nchar(sub.temp.student.df$groups.grade)), "id"]
                    if (length(candidate.ids) == n.grades.per.group.per.group){
                        sampled.ids <- candidate.ids
                    } else {
                        sampled.ids <- sample(candidate.ids, size = min(length(candidate.ids),
                                                                        n.grades.per.group.per.group))
                    }
                } else {
                    candidate.ids <- sub.temp.student.df[nchar(sub.temp.student.df$groups.grade) == min(nchar(sub.temp.student.df$groups.grade)), "id"]
                    n.left <- length(sub.temp.student.df[nchar(sub.temp.student.df$groups.grade) !=
                                                                      min(nchar(sub.temp.student.df$groups.grade)),
                                                                      "id"])
                    if (n.left > 0){
                        sampled.ids <- c(candidate.ids,
                                           sample(sub.temp.student.df[nchar(sub.temp.student.df$groups.grade) !=
                                                                      min(nchar(sub.temp.student.df$groups.grade)),
                                                                      "id"], min(c(n.left, n.grades.per.group.per.group -
                                                                             length(candidate.ids)))))
                    } else {
                        sampled.ids <- candidate.ids
                    }
                }
                student.df$groups.grade[student.df$id %in% sampled.ids] <-
                    paste0(student.df$groups.grade[student.df$id %in% sampled.ids], substr(i, nchar(i), nchar(i)))
            }
        }
    }
    ## Second pass: shuffling the students and then allocating spare
    ## spots on a one-by-one basis.
    group.names <- substr(groups.df$name, nchar(groups.df$name), nchar(groups.df$name))
    for (i in sample(n.students)){
        for (j in seq_along(numeric(n.grades - nchar(student.df$groups.grade[i])))){
            ## Calculating number of times each group has been allocated to a grader.
            grade.counts <- letter.count(student.df$groups.grade, group.names)
            candidate.groups <- group.names[group.names != substr(student.df$group.name[i],
                                                                  nchar(student.df$group.name[i]),
                                                                  nchar(student.df$group.name[i])) &
                                            !(group.names %in% strsplit(student.df$groups.grade[i], split = "")[[1]])]
            grade.counts <- grade.counts[candidate.groups]
            grade.counts <- grade.counts[grade.counts == min(grade.counts)]
            candidate.groups <- names(grade.counts)
            student.df$groups.grade[i] <- paste0(student.df$groups.grade[i], sample(candidate.groups, size = 1))
        }
    }
    student.df
}

## Function to count the number of letters in a vector.
letter.count <- function(x, group.names){
    table(factor(strsplit(paste(x, collapse = ""), split = "")[[1]], levels = group.names))
}

team.count <- function(group.category.names, stream, course.id, domain){
    url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
    streams.df <- get.data(url)
    if (stream == "tuesday"){
        stream.id <- streams.df$id[streams.df$name == "STATS/DATASCI 399 - Tuesday Stream"]
    } else if (stream == "friday"){
        stream.id <- streams.df$id[streams.df$name == "STATS/DATASCI 399 - Friday Stream"]   
    } else if (stream == "online"){
        stream.id <- streams.df$id[streams.df$name == "STATS/DATASCI 399 - Online"]
    } else if (stream == "swu"){
        stream.id <- streams.df$id[streams.df$name == "DATASCI 399 - Online SWU"]
    }
    ## Getting student information for this stream.
    url <- paste(domain, "/api/v1", "groups", stream.id, "users", sep = "/")
    student.df <- get.data(url)
    n.students <- nrow(student.df)
    student.names <- student.df$name
    student.ids <- student.df$id
    ## Creating output matrix.
    out <- matrix(0, nrow = n.students, ncol = n.students)
    ## Getting the group category ID number.
    url <- paste(domain, "/api/v1", "courses", course.id, "group_categories", sep = "/")
    group.category.df <- get.data(url)
    ## Number of group categories.
    n.group.categories <- length(group.category.names)
    ## Looping over the group categories.
    for (g in 1:n.group.categories){
        group.category.id <- group.category.df$id[group.category.df$name == group.category.names[g]]
        ## Getting group information for this category.
        url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
        group.df <- get.data(url)
        group.df <- group.df[group.df$group_category_id == group.category.id, ]
        ## Removing the "Absent" group.
        group.df <- group.df[group.df$name != "Absent", ]
        ## All the group IDs.
        group.ids <- group.df$id
        ## Creating a vector of group id for each student.
        student.group.id <- numeric(n.students)
        ## Getting users for a group.
        for (i in group.ids){
            url <- paste(domain, "/api/v1", "groups", i, "users", sep = "/")
            user.df <- get.data(url)
            student.group.id[student.df$id %in% user.df$id] <- i
        }
        ## Incrementing the correct entries of the output matrix.
        for (i in 1:n.students){
            out[i, which(student.group.id == student.group.id[i])] <-
                out[i, which(student.group.id == student.group.id[i])] + 1
        }
    }
    rownames(out) <- student.names
    colnames(out) <- student.names
    out
}

## A function to summarise peer-appraisals.
summarise.pa <- function(assignment.ids, assignment.pa.ids, domain = "https://canvas.auckland.ac.nz", course.id){
    ## Getting student list.
    url <- paste(domain, "/api/v1", "courses", course.id, "users", sep = "/")
    pa.df <- get.data(url)[, c("id", "name")]
    pc.df <- pa.df
    n.people <- nrow(pa.df)
    n.assignments <- length(assignment.pa.ids)
    pa.mat <- matrix(0, nrow = n.people, ncol = n.assignments)
    pc.mat <- matrix(0, nrow = n.people, ncol = n.assignments)
    for (i in 1:n.assignments){
        ## Getting assignment information.
        url <- paste(domain, "/api/v1", "courses", course.id, "assignments", assignment.ids[i], sep = "/")
        assign.info <- get.data(url)
        ## Getting group category ID.
        group.category.id <- assign.info$group_category_id
        ## Getting group category ID.
        group.category.id <- assign.info$group_category_id
        ## Getting group category information.
        url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
        group.category.df <- get.data(url)
        group.category.df <- group.category.df[group.category.df$group_category_id == group.category.id, ]
        n.groups <- nrow(group.category.df)
        ## Getting information for the different groups.
        group.ids <- group.category.df$id
        n.groups <- nrow(group.category.df)
        group.grades <- vector(mode = "list", length = n.groups)
        group.grades[1:n.groups] <- 50
        names(group.grades) <- paste("Group", LETTERS[1:n.groups])
        grades.list <- calc.grades(assignment.id = assignment.ids[i], assignment.pa.id = assignment.pa.ids[i],
                                     group.grades = group.grades, grade.fun = function(x, y) x, domain = domain,
                                   course.id = course.id)
        individual.df <- grades.list$individual[, c("id", "stream.name", "name", "pa.score", "p.completed")]
        pa.df <- merge(pa.df, individual.df[, c("id", "stream.name"[i == 1], "pa.score")], by = "id", all = TRUE, sort = FALSE)
        pc.df <- merge(pc.df, individual.df[, c("id", "stream.name"[i == 1], "p.completed")], by = "id", all = TRUE, sort = FALSE)
        names(pa.df)[3 + i] <- paste0("pa.score.", i)
        names(pc.df)[3 + i] <- paste0("p.completed.", i)
    }
    pa.df <- pa.df[apply(pa.df, 1, function(x) any(!is.na(x[-(1:2)]))), ]
    pc.df <- pc.df[apply(pa.df, 1, function(x) any(!is.na(x[-(1:2)]))), ]
    pa.df$average <- apply(pa.df[, -(1:3)], 1, mean, na.rm = TRUE)
    list(pa = pa.df, pc = pc.df)
}
