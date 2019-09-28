package com.assignment

import com.assignment.Gender.Gender
import com.assignment.models.{ScoreCard, Student}

import scala.collection.immutable

object q3 extends App {

  case class Student(id: Long, name: String, gender: Gender)

  val students = List(
    Student(1, "Mahesh", Gender.male),
    Student(2, "Shiva", Gender.female),
    Student(3, "M", Gender.male)
  )

  val scoreCards = List(
    ScoreCard(1, Map(101 -> 79, 102 -> 90, 103 -> 86, 104 -> 74, 105 -> 99), 86),
    ScoreCard(3, marks = Map(301 -> 89, 302 -> 95, 303 -> 83, 304 -> 71, 305 -> 95), 86),
    ScoreCard(2, marks = Map(101 -> 89, 102 -> 95, 103 -> 83, 104 -> 71, 105 -> 95), 86)
  )

  /*
  Write a method getScoreCardByGender to return a tuple of ScoreCards
   (e.g. (List[ScoreCard], List[ScoreCard])), where first field in the tuple
    has male student's scorecard and the second field has female student's scorecards.
   */

  def getScoreCardByGender(): (List[ScoreCard], List[ScoreCard])  =  {

    val studentMarks: List[(Long, Gender, ScoreCard)] = for{
      student <- students
      scoreCard <- scoreCards
      if student.id == scoreCard.studentId
    } yield (student.id, student.gender, scoreCard)

    val maleSc: List[ScoreCard] = studentMarks.filter(gender => gender._2 == Gender.male).map(tuple => tuple._3)

    val femaleSc: List[ScoreCard] = studentMarks.filter(gender => gender._2 == Gender.female).map(tuple  => tuple._3)

    (maleSc, femaleSc)
  }

  println(getScoreCardByGender())
}
