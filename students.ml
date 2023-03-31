

type student = {
 firstName : string;
 lastName: string;
 id: int;
 currentSemester: int;
 grades: (int*float) list;
}

let s1:student = {
  firstName= "John";
  lastName= "Smith";
  id= 1;
  currentSemester= 10;
  grades= [(1,5.5); (3,3.0)]
}


let s2:student = {
  firstName= "James";
  lastName= "West";
  id= 2;
  currentSemester= 10;
  grades= [(1,4.5); (3,2.8)]
}

let s3:student = {firstName= "Jack";
lastName= "Jackson";
id= 3;
currentSemester= 4;
grades= [(1,5.0); (2,2.1); (3, 10.2)]}



type database = student list;;

let myDB:(student list)= [s1; s2; s3];;

let insert (s: student) db:database = s::db;;




let rec find_by_id (is:int) (db:database) = match db with
  | [] -> failwith "Student with the ID couldn't be found"
  | x::xs -> if x.id == is then x else find_by_id is xs;;


  let rec find_by_last_name (la:string) (db:database) = match db with 
  | [] -> []
  | x::xs -> if x.lastName = la then [x]::find_by_last_name la xs else find_by_last_name la xs;;





let remove_by_id id db = 
  
  let rec remove_with_id id db emptyDB = 
    match db with 
    | [] -> []
    | x::xs -> if id == x.id then remove_with_id id xs emptyDB else emptyDB@[x]::remove_with_id id xs emptyDB
    in 
    remove_with_id id db [];;



let student_avg_grade id db = 
    let myStudent = find_by_id id db 
    in 

    let rec insiderFunction grades (gradeCount:float) (semesterCounter:float) = 
      match grades with 
    | [] -> if gradeCount = 0.0 && semesterCounter = 0.0 then 0.0 else gradeCount /. semesterCounter
    | (_, grade)::xs -> insiderFunction (xs) (gradeCount+.grade) (semesterCounter +. 1.0)
    in insiderFunction myStudent.grades (0.0) (0.0)





    let semester_avg_grade semester data = 
      let rec getSpecificSemesters semester data grades:((int*float) list)= 
        match data with 
        |[] -> grades
        |x::xs ->
            let rec gatherGrades studentGrades =
              match studentGrades with 
              |[] -> []
              |(s, g)::tl -> if s=semester then grades@[(s,g)] else
                    gatherGrades tl 
            in 
            getSpecificSemesters semester xs (gatherGrades (x.grades)) 
          
      in
      let grades =  getSpecificSemesters semester data []
      in

      let rec get_avg_of_semester_grades grades (points:float) (count:float) =
        match grades with 
        |[] -> points /. count
        |(_, g)::tl -> get_avg_of_semester_grades (tl) (points +. g) (count+.1.0)
      in 
      let final = get_avg_of_semester_grades grades (0.0) (0.0) in
      if final > 0.0 then final else 0.0
      
      
      
      
                  