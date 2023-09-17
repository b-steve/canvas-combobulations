library(httr)
library(jsonlite)
library(readxl)

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

## A function to get a data frame of student groupings.
get.groups <- function(course.id, group.category.id, domain = "https://canvas.auckland.ac.nz"){
    ## Getting a data frame of group information.
    url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
    group.df <- get.data(url)
    group.df <- group.df[group.df$group_category_id == group.category.id, ]
    group.names <- group.df$name
    group.ids <- group.df$id
    n.groups <- length(group.ids)
    ## Creating group membership data frames.
    group.membership.dfs <- vector(mode = "list", length = 4)
    for (i in 1:n.groups){
        url <- paste(domain, "/api/v1", "groups", group.ids[i], "users", sep = "/")
        group.membership.dfs[[i]] <- data.frame(get.data(url)[, c(1, 2)], stream = group.names[i])
    }
    do.call(rbind, group.membership.dfs)
}

## A function to get stream groupings.
get.streams <- function(course.id, domain = "https://canvas.auckland.ac.nz"){
    ## Getting the group category ID number.
    url <- paste(domain, "/api/v1", "courses", course.id, "group_categories", sep = "/")
    group.category.df <- get.data(url)
    group.category.id <- group.category.df$id[group.category.df$name == "Stream"]
    ## Getting a data frame of group information.
    get.groups(course.id, group.category.id, domain)
}

## A function to get student data.
get.user.data <- function(course.id, domain = "https://canvas.auckland.ac.nz"){
    url <- paste(domain, "/api/v1", "courses", course.id, "users", sep = "/")
    get.data(url)
}

## A function to create groups.
create.groups <- function(group.names, group.category.name, course.id, domain = "https://canvas.auckland.ac.nz"){
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
        cmd <- paste("curl", url, "-F", name.arg, "-F", public.arg, "-F", join.arg, "-H", auth.arg, "> /dev/null")
        system(cmd)
    }
}

## A function to add a member to a group.
add.member <- function(group.id, user.id, domain = "https://canvas.auckland.ac.nz"){
    url <- paste(domain, "/api/v1", "groups", group.id, "memberships", sep = "/")
    user.arg <- paste0("\'user_id=", user.id, "\'")
    auth.arg <- paste0("\'Authorization: Bearer ", token, "\'")
    cmd <- paste("curl", url, "-F", user.arg, "-H", auth.arg, "> /dev/null")
    system(cmd)
}

post.grade <- function(grade, assignment.id, user.id, course.id, domain = "https://canvas.auckland.ac.nz"){
    url <- paste(domain, "/api/v1", "courses", course.id, "assignments",
                 assignment.id, "submissions", "update_grades", sep = "/")
    auth.arg <- paste0("\'Authorization: Bearer ", token, "\'")
    post.arg <- paste0("\'grade_data[", user.id, "][posted_grade]=", grade, "%\'")
    cmd <- paste("curl", url, "-X POST", "-F", post.arg, "-H", auth.arg, "> /dev/null")
    system(cmd)
}

## A function to post individual grades. Just need a named vector.
post.individual.grades <- function(grades, assignment.id, course.id, domain = "https://canvas.auckland.ac.nz"){
    ## Getting student list.
    url <- paste(domain, "/api/v1", "courses", course.id, "users", sep = "/")
    people.df <- get.data(url)
    student.names <- names(grades)
    n.students <- length(grades)
    for (i in 1:n.students){
        user.id <- people.df$id[people.df$short_name == student.names[i]]
        post.grade(grades[i], assignment.id, user.id, course.id, domain)
    }
}

## A function to assign only one stream to an assignment.
assign.to.stream <- function(course.id, assignment.id, stream.name, domain = "https://canvas.auckland.ac.nz"){
    ## Sanitising input.
    stream.name.sanitised <- gsub("ā", "a", stream.name)
    ## Getting stream information.
    stream.df <- get.streams(course.id, domain)
    student.id <- stream.df$id[stream.df$stream == stream.name.sanitised]
    ## Creating overrides for the students in the stream.
    url <- paste(domain, "/api/v1", "courses", course.id, "assignments", assignment.id, "overrides", sep = "/") 
    F.ids.arg <- paste(paste0("-F \'assignment_override[student_ids][]=" , student.id, "\'"), collapse = " ")
    title.arg <- paste0("\'assignment_override[title]=", "streamassign", "\'")
    auth.arg <- paste0("\'Authorization: Bearer ", token, "\'")
    cmd <- paste("curl", url, "-X POST", F.ids.arg, "-F", title.arg, "-H", auth.arg, "> /dev/null")
    system(cmd)
    ## Making assignment only visible to overrides.
    url <- paste(domain, "/api/v1", "courses", course.id, "assignments", assignment.id, sep = "/")
    cmd <- paste("curl", url, "-X PUT -F \'assignment[only_visible_to_overrides]=true\' -H", auth.arg, "> /dev/null")
    system(cmd)
}


## Creates assignments for the four streams and ensures each is
## available to the correct students.
create.assignment <- function(assignment.name, streams = c("Hihi", "Ruru", "Whio", "Kākā"), assignment.group.name, course.id, domain = "https://canvas.auckland.ac.nz"){
    ## Getting assignment group ID, if name is provided.
    url <- paste(domain, "/api/v1", "courses", course.id, "assignment_groups", sep = "/")
    assignment.group.df <- get.data(url)
    assignment.group.id <- assignment.group.df$id[assignment.group.name == assignment.group.df$name]
    auth.arg <- paste0("\'Authorization: Bearer ", token, "\'")
    ## Creating an assignment for each stream.
    n.streams <- length(streams)
    for (i in 1:n.streams){
        ## URL for the curl POST.
        url <- paste(domain, "/api/v1", "courses", course.id, "assignments", sep = "/")
        ## Creating assignment name.
        full.assignment.name <- paste0(streams[i], ": ", assignment.name)
        cmd <- paste("curl", url, "-X POST -F", paste0( "\'assignment[name]=", full.assignment.name, "\'"), paste0( "-F \'assignment[assignment_group_id]=", assignment.group.id, "\'"), "-H", auth.arg, "> /dev/null")
        system(cmd)
        ## Getting assignment ID for the newly created assignment.
        url <- paste(domain, "/api/v1", "courses", course.id, "assignment_groups", assignment.group.id, "assignments", sep = "/")
        assignment.df <- get.data(url)
        assignment.id <- assignment.df$id[assignment.df$name == full.assignment.name]
        assign.to.stream(course.id, assignment.id, streams[i])
    }
}

## A function to copy a Hihi assignment description and use it for
## Ruru, Whio, and Kākā descriptions, creating these assignments if
## necessary.
copy.assignment <- function(assignment.id, orig.stream = "Hihi", other.streams = c("Ruru", "Whio", "Kākā"), course.id, domain = "https://canvas.auckland.ac.nz"){
    auth.arg <- paste0("\'Authorization: Bearer ", token, "\'")
    ## Getting information about original assignment.
    url <- paste(domain, "/api/v1", "courses", course.id, "assignments", assignment.id, sep = "/")
    orig.assign.df <- get.data(url)
    orig.assign.name <- orig.assign.df$name
    orig.assign.stem <- strsplit(orig.assign.name, ":")[[1]][1]
    ## Checking if the original assignment name begins with the original stream name.
    if (orig.assign.stem != orig.stream){
        stop("The assignment name must start with the original stream name, followed by a colon.")
    }
    ## Getting the rest of the assignment name, following the original stream name.
    assign.name.suffix <- substr(orig.assign.name, nchar(orig.assign.stem) + 1, nchar(orig.assign.name))
    ## Getting a list of all course assignment names.
    url <- paste(domain, "/api/v1", "courses", course.id, "assignments", sep = "/")
    all.assign.df <- get.data(url)
    n.other.streams <- length(other.streams)
    ## Getting original description.
    orig.description <- orig.assign.df$description
    for (i in 1:n.other.streams){
        ## Getting ID for other assignment.
        other.assignment.id <- all.assign.df$id[all.assign.df$name == paste0(other.streams[i], assign.name.suffix)]
        ## Updating description.
        new.description <- gsub(orig.stream, other.streams[i], orig.description)
        new.description <- gsub(tolower(orig.stream), tolower(other.streams[i]), new.description)
        new.description <- gsub(toupper(orig.stream), toupper(other.streams[i]), new.description)
        new.description <- gsub("\"", "\\\\\"", new.description)
        new.description <- gsub("\'", "<span>&#39;</span>", new.description)
        ## Editing new assignment description.
        url <- paste(domain, "/api/v1", "courses", course.id, "assignments", other.assignment.id, sep = "/")
        cmd <- paste("curl", url, "-X PUT -F", paste0("\'assignment[description]=\"", new.description, "\"\'"), "-H", auth.arg, "> /dev/null")
        system(cmd)
    }
}

## The main function to calculate individual grades.
## assignment.id: The Canvas ID for the main assignment.
## group.grades: A list of group grades, with group names matching those on Canvas.
## appraisal.scores: A data frame extracted from the FeedbackFruits analytics.
## grade.fun: A function to determine grades based on a group assessment and peer assessments.
## post: Whether or not to post grades to Canvas; either TRUE or FALSE. If NULL, the user is prompted. 
## domain: The Canvas domain for your institution.
## course.id: The Canvas ID for the course.
calc.grades <- function(assignment.id, group.grades = NULL, appraisal.scores, grade.fun = NULL, post = NULL, course.id, domain = "https://canvas.auckland.ac.nz"){
    ## Extracting variables from appraisal scores.
    student.name <- appraisal.scores[, 2]
    student.group <- sapply(strsplit(appraisal.scores[, 1], ": "), function(x) x[2])
    n.students <- length(student.name)
    n.groups <- length(group.grades)
    student.id <- numeric(n.students)
    ## Getting the user IDs, rather than the SIS user IDs.
    user.df <- get.user.data(course.id, domain)
    for (i in 1:n.students){
        student.id[i] <- user.df$id[user.df$name == student.name[i]]
    }
    ## Calculating appraisal scores.
    all.appraisal.score <- appraisal.scores[, 5:ncol(appraisal.scores)]
    student.appraisal.score <- apply(all.appraisal.score, 1, mean, na.rm = TRUE)
    ## Calculating final grades.
    student.group.grade <- numeric(n.students)
    student.final.grade <- numeric(n.students)
    for (i in 1:n.groups){
        group.name <- names(group.grades[i])
        group.grade <- group.grades[[i]]
        student.group.grade[student.group == group.name] <- group.grade
        student.final.grade[student.group == group.name] <-
            round(calculate.grades(group = group.grade,
                                   individual = student.appraisal.score[student.group == group.name])
                  , 1)
    }
    ## Posting grades.
    if (is.null(post)){
        print(data.frame(name = student.name, group = student.group.grade,
                         individual = student.appraisal.score, final = student.final.grade)[order(student.final.grade), ])
        post <- readline(prompt = "Post grades? Type 'yes' to confirm.")
    } else {
        if (post){
            post  <- "yes"
        }
    }
    if (post == "yes"){
        for (i in 1:n.students){
            post.grade(student.final.grade[i], assignment.id, student.id[i], course.id, domain)
        }
    }
}

## A function to randomly allocate students to groups.

## group.size: The number of students per group. A small number of
##             groups might be larger.
## group.category.name: The name of the group category for the
##                      assignment in Canvas.
## stream: The stream name. This needs to match what appears in
##         Canvas. If a single stream is split into substreams, then
##         the substream names in Canvas must end with a number.
## prefix: A prefix to add to the group name.
## absent: A vector of user IDs for students who are absent. Students
##         who are absent are removed prior to random allocation, and
##         get put in their own group with other absentees.
## no.allocation: If TRUE, then students aren't allocated to
##                groups. All that happens is that empty groups are
##                created on Canvas. This is useful if you want to
##                manually allocate groups, but don't want to
##                tediously create them all on Canvas.
## WARNING: You can't have more than 26 groups in a substream, sorry lmao.
allocate.groups <- function(group.size, group.category.name, stream, prefix = "", absent = integer(0), no.allocation = FALSE, course.id, domain = "https://canvas.auckland.ac.nz"){
    url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
    streams.df <- get.data(url)
    stream.id <- streams.df$id[streams.df$name == stream]
    ## Stripping substream number from stream name.
    stream <- gsub('[[:digit:]]+', '', stream)
    ## Getting student information for this stream.
    url <- paste(domain, "/api/v1", "groups", stream.id, "users", sep = "/")
    student.df <- get.data(url)
    absent <- absent[absent %in% student.df$id]
    student.df <- student.df[!(student.df$id %in% absent), ]
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
    existing.substream.group.names <- groups.df$name[substr(groups.df$name, 1, 1 + nchar(prefix) + nchar(stream)) == paste(prefix, stream)]
    if (length(existing.substream.group.names) == 0){
        start.letter <- "A"
    } else {
        start.letter <- LETTERS[!(LETTERS %in% substr(existing.substream.group.names,
                                                      nchar(existing.substream.group.names),
                                                      nchar(existing.substream.group.names)))][1]
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
    for (i in 1:n.groups){
        out[[i]] <- shuffled.names.mat[i, ][!is.na(shuffled.names.mat[i, ])]
    }
    start.letter.number <- which(LETTERS == start.letter)
    names(out) <- paste(prefix, stream, LETTERS[start.letter.number:(start.letter.number + n.groups - 1)])
    print(out)
    post <- readline(prompt = "Post groups? Type 'yes' to confirm.")
    if (post == "yes"){
        create.groups(names(out), group.category.name, course.id, domain)
        url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
        groups.df <- get.data(url)
        groups.df <- groups.df[groups.df$group_category_id == group.category.id, ]
        if (length(absent) > 0){
            if (!any(groups.df$name == "Absent")){
                create.groups("Absent", group.category.name, course.id, domain)
            }
            url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
            groups.df <- get.data(url)
            groups.df <- groups.df[groups.df$group_category_id == group.category.id, ]
            group.id <- groups.df$id[groups.df$name == "Absent"]
            for (i in absent){
                add.member(group.id, i, domain)
            }
        }
        if (!no.allocation){
            for (i in 1:nrow(shuffled.ids.mat)){
                group.id <- groups.df$id[groups.df$name == names(out)[i]]
                for (j in 1:ncol(shuffled.ids.mat)){
                    user.id <- shuffled.ids.mat[i, j]
                    if (!is.na(user.id)){
                        add.member(group.id, user.id, domain)
                    }
                }
            }
        }
    }
}

## Function to count the number of letters in a vector.
letter.count <- function(x, group.names){
    table(factor(strsplit(paste(x, collapse = ""), split = "")[[1]], levels = group.names))
}

team.count <- function(group.category.names, stream, course.id, domain = "https://canvas.auckland.ac.nz"){
    ## Getting student information for the stream.
    url <- paste(domain, "/api/v1", "courses", course.id, "groups", sep = "/")
    streams.df <- get.data(url)
    stream.base <- stream <- gsub('[[:digit:]]+', '', stream)
    stream.ids <- streams.df$id[gsub('[[:digit:]]+', '', streams.df$name) == gsub('[[:digit:]]+', '', stream)]
    student.df <- data.frame()
    for (i in stream.ids){
        url <- paste(domain, "/api/v1", "groups", i, "users", sep = "/")
        student.df <- rbind(student.df, get.data(url)[, c("sis_user_id", "id", "name")])
    }
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

## A function to summarise teammate-appraisals across multiple activities.
## dir: A directory including FeedbackFruits "analytics" .xslx files.
summarise.ta <- function(dir, course.id, domain = "https://canvas.auckland.ac.nz"){
    ## Identifying files with appraisal data.
    all.files <- list.files(path = dir, pattern = ".xlsx")
    activity.name <- unlist(strsplit(all.files, ".xlsx"))
    n.activities <- length(activity.name)
    ## Getting student list with stream allocations.
    student.df <- get.streams(course.id)
    n.students <- nrow(student.df)
    ## Reading in appraisal data.
    received.dfs <- vector(mode = "list", length = n.activities)
    given.dfs <- vector(mode = "list", length = n.activities)
    for (i in 1:n.activities){
        received.dfs[[i]] <- as.data.frame(read_xlsx(paste0(dir, "/", all.files[i]), sheet = 6))
        given.dfs[[i]] <- as.data.frame(read_xlsx(paste0(dir, "/", all.files[i]), sheet = 5))
    }
    ## Summarising appraisal data for each individual student.
    received.scores <- matrix(0, nrow = n.students, ncol = n.activities)
    colnames(received.scores) <- activity.name
    rownames(received.scores) <- student.df$name
    given.scores <- matrix(0, nrow = n.students, ncol = n.activities)
    colnames(given.scores) <- activity.name
    rownames(given.scores) <- student.df$name
    for (i in 1:n.students){
        student.name <- student.df[i, ]$name
        received.scores[i, ] <- sapply(lapply(received.dfs, function(x) as.matrix(x[x[, 2] == student.name, 5:ncol(x)])), mean, na.rm = TRUE)
        given.scores[i, ] <- sapply(lapply(given.dfs, function(x) as.matrix(x[x[, 2] == student.name, 8:ncol(x)])), mean, na.rm = TRUE)
    }
    list(received = data.frame(student.df, received.scores, overall = apply(received.scores, 1, mean, na.rm = TRUE),
                               trim.overall = apply(received.scores, 1, trim.mean)),
         given = data.frame(student.df, given.scores, overall = apply(given.scores, 1, mean, na.rm = TRUE)))
}

## A function to drop off the lowest rating and compute a mean.
trim.mean <- function(x){
    n <- length(x)
    x <- sort(x, decreasing = TRUE, na.last = TRUE)[-n]
    mean(x, na.rm = TRUE)
}
