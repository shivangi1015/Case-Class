package com.assignment

import java.io

import com.assignment.models.{ScoreCard, Student}

object q2 extends App {

  /*
  Write a method which takes input as student name and print the score cards.
   If it finds one or more than one score card print all of them other wise
   print "No data found". The print should be in increasing order of the student id.
   */

  def generateScore(studentName: String): Any = {

    val students = List(
      Student(1, "Mahesh"),
      Student(2, "Shiva"),
      Student(3, "Shiva")

    )
    val scoreCards = List(
      ScoreCard(1, Map(101 -> 79, 102 -> 90, 103 -> 86, 104 -> 74, 105 -> 99), 86),
      ScoreCard(3, marks = Map(301 -> 89, 302 -> 95, 303 -> 83, 304 -> 71, 305 -> 95), 86),
      ScoreCard(2, marks = Map(101 -> 89, 102 -> 95, 103 -> 83, 104 -> 71, 105 -> 95), 86)

    )

    val studentMarks: List[(String, ScoreCard)] = for {
      student <- students
      scoreCard <- scoreCards
      if student.id == scoreCard.studentId
    } yield student.name -> scoreCard

    studentMarks.groupBy(x => x._1).map {
      case (k, v) if v.length > 1 => (k, v.map { case (_, s) => s })
      case (k, v) => (k, v.head._2)

    }
      .getOrElse(studentName, "No Data found")

  }

  println(generateScore("Shiva"))
  println(generateScore("s"))
}
