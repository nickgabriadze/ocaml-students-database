type student = {
  firstName : string;
  lastName : string;
  id : int;
  currentSemester : int;
  grades : (int * float) list;
}
val s1 : student
val s2 : student
val s3 : student
type database = student list
val myDB : student list
val insert : student -> student list -> database
val find_by_id : int -> database -> student
val find_by_last_name : string -> database -> student list list
val remove_by_id : int -> student list -> student list list
val student_avg_grade : int -> database -> float
val semester_avg_grade : int -> student list -> float
