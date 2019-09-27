package com.assignment

import com.assignment.models.{ScoreCard, Student}

import scala.collection.immutable

object q1 extends App {

  /*
  Write a method which takes no parameter and generates a Map with key student name
   and value as ScoreCard. As there can be more than one student with same name,
    the logic we have to follow is that, if two or more student has same name the
     key should be the name of the student and the values (ScoreCards) should be
     in a List, otherwise the key should be the student name and value should be
      the case class ScoreCard. e.g. Map should be Map[String, AnyRef].
   */
  def generateScore(): List[Map[String, AnyRef]] = {

    val students = List(
      Student(1, "Mahesh"),
      Student(2, "Shiva"),
      Student(3, "Shiva")

    )
    val scoreCards = List(
      ScoreCard(1, Map(101 -> 79, 102 -> 90, 103 -> 86, 104 -> 74, 105 -> 99), 86),
      ScoreCard(2, marks = Map(101 -> 89, 102 -> 95, 103 -> 83, 104 -> 71, 105 -> 95), 86),
      ScoreCard(3, marks = Map(101 -> 89, 102 -> 95, 103 -> 83, 104 -> 71, 105 -> 95), 86)

    )

    val studentMarks: List[(String, ScoreCard)] = for{
      student <- students
      scoreCard <- scoreCards
      if student.id == scoreCard.studentId
    } yield student.name -> scoreCard

    val studentNames: List[String] = studentMarks.map(s => s._1)

    if(studentNames.distinct.lengthCompare(studentNames.size) == 0) {
      // No student has same name
      println("inside if")
      val re: List[Map[String, ScoreCard]] =  for{
        student <- students
        scoreCard <- scoreCards
        if student.id == scoreCard.studentId
      } yield Map(student.name -> scoreCard)

      println(re)
      re
    } else {
      //student has same name
      println("inside else")

      println(":: " + studentMarks.map(x => x._1).groupBy(identity).filter(x => x._2.size > 1))
     /* val s: Map[String, List[String]] = studentMarks.map(x => x._1).groupBy(identity).filter(x => x._2.size > 1)
      val s1: immutable.Iterable[String] = studentMarks.map(x => x._1).groupBy(identity).filter(x => x._2.size > 1).map(x => x._1)
     println("id: " + s1)
      println(":: " + studentMarks.map(x => x._1).groupBy(identity).filter(x => x._2.size > 1).map(x => x._1))
      val grpby = studentNames.groupBy(identity)
      println("grp: " + grpby)
      val dist = grpby.filter(x => x._2.size > 1).map(x => x._1)
      println("dist: " + dist)*/

      val studentMarksAndStudentId: List[((String, Long), ScoreCard)] = for{
        student <- students
        scoreCard <- scoreCards
        if student.id == scoreCard.studentId
      } yield (student.name, student.id )-> scoreCard
      println("studentMarksAndStudentId:: " + studentMarksAndStudentId)

      val l = for{
        s <- studentMarksAndStudentId
        scoreCard <- scoreCards
        if s._1._2 == scoreCard.studentId
      } yield Map(s._1._1 -> scoreCard)

      println("lets see::")
      println(l)

      for{
        student <- students
        scoreCard <- scoreCards
      } yield Map(student.name -> scoreCard)
    }
/*
    val listOfStudents: List[Student] = List(Student(1, "shivangi"), Student(2, "mahesh"),
      Student(2, "mahesh"))
//    val listOfScoreCard = List(ScoreCard(1, Map(1L -> 1), 70), ScoreCard(2, Map(2L -> 2), 80))

    val studentNames = listOfStudents.map(x => x.name)
    if(studentNames.lengthCompare(unique(studentNames).size) == 0) {
      // No student has same name
      Map()
    } else {
      //student has same name

    }*/
  }

  def unique(list: List[String]): List[String] = {
    def inner(rem: List[String], uniqueList: List[String]): List[String] = {
      rem match {
        case h :: tail if !(uniqueList contains h) => inner(tail, uniqueList :+ h)
        case _ :: tail => inner(tail, uniqueList)
        case Nil => uniqueList
      }
    }
    inner(list, Nil)
  }

  val r = generateScore()
  println(r)
}
