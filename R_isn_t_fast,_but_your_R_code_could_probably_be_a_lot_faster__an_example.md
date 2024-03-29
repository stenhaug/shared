R isn’t fast, but your R code could probably be a lot faster: an example
================
Ben Stenhaug

A friend of mine was recently complaining that R is slow. It’s true that
R isn’t the fastest language, but it’s also true that the gap between
someone’s slow R code and the optimal code in another language is
usually mostly bridged by writing better R code.

# The Example

My friend gave me the following description of his problem:

> “One file is one line per student demographic data in the entire
> district. the other is a transcript file that’s one line per course
> taken by each student in the district over their entire time in the
> district. So there are ~7500 students, but the transcript file has
> ~325,000 entries. my goal is to make a dichotomous variable for each
> math (~850 individual classes) and ELA class (~50 individual classes)
> offered in the district, add them as columns to the demographic
> dataset and give a student a 1 or a 0 if they took each class.”

He also sent me his current code:

``` r
demo.credits = read.csv("demo.credits.csv", header = TRUE)
transcript.demo = read.csv("transcript-demo.csv", header = TRUE)

math.courses <- unique(transcript.demo$coursename[transcript.demo$credittype=="Math"])
ela.courses <- unique(transcript.demo$coursename[transcript.demo$credittype=="English"])
courses <- unlist(list(math.courses,ela.courses))

classes <- matrix(NA,length(demo.credits$studentnumber),length(courses))
colnames(classes) <- courses

for (i in 1:length(demo.credits$studentnumber)){
    class.list <- transcript.demo$coursename[transcript.demo$studentnumber==demo.credits$studentnumber[i]]
    for (j in 1:length(courses)){
        classes[i,j] <- ifelse(sum(class.list==courses[j])>1,1,0)
    }
    cat("student", i, "completed, ", (length(demo.credits$studentnumber)-i), "students remaining\n")
    flush.console()
}

demo.credits.class <- cbind(demo.credits, classes)
write.csv(demo.credits.class, "demo-credits-class.csv")
```

# Speeding It Up

The first thing I notice when looking at his code is that he approached
the problem — with nested loops and the necessary logic — in a way that
works well for other languagues, but isn’t the best for R. In general, R
is best if you can write vectorized code as opposed to loops. Bonus
points for keeping data in rectangular data frames and using the tools
of the tidyverse.

The second thing I notice is that I don’t have access to his data. So I
create simple data with the necessary characteristics:

``` r
demo <- 
   tribble(
      ~studentnumber,  ~sex,
      1, "male",
      2, "female",
      3, "male"
   )

transcript <- 
   tribble(
      ~studentnumber, ~classname, ~credittype,
      1, "algebra", "math",
      1, "poetry", "english",
      2, "poetry", "english",
      3, "algebra", "math"
   )
```

Now I can work to solve the problem. My approach is to first make a
single variable for class which is the credit type (math or english)
followed by the class name (algebra for example). I think this makes
things easier to read our results.

I then use spread which takes a column of keys (the classes) and values
(I make an indicator of 1 because if the student is in the dataset, they
took the course) and then spreads those two columns wide into a column
for each unique value in the key column.

Usually combinations not seen in the dataset are left as NA but in this
case I fill with 0 to encode that the student did not take the course.
Notice that I also make

``` r
transcript_wide <- 
   transcript %>% 
   mutate(
      class = paste0(credittype, "_", classname), 
      indicator = 1
   ) %>% 
   select(studentnumber, class, indicator) %>% 
   spread(class, indicator, fill = 0)
```

Now all we need to do is join with our demographics data, and we have
what we’re looking for\!

``` r
final <- 
   demo %>% 
   left_join(transcript_wide, by = "studentnumber")

final
```

    ## # A tibble: 3 x 4
    ##   studentnumber sex    english_poetry math_algebra
    ##           <dbl> <chr>           <dbl>        <dbl>
    ## 1             1 male                1            1
    ## 2             2 female              1            0
    ## 3             3 male                0            1
