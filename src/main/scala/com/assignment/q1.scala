package com.assignment

import com.assignment.models.ScoreCard
import com.assignment.models.models.Student

object q1 extends App {

  /*
  Write a method which takes no parameter and generates a Map with key student name
   and value as ScoreCard. As there can be more than one student with same name,
    the logic we have to follow is that, if two or more student has same name the
     key should be the name of the student and the values (ScoreCards) should be
     in a List, otherwise the key should be the student name and value should be
      the case class ScoreCard. e.g. Map should be Map[String, AnyRef].
   */
  def generateScore(): Map[String, AnyRef] = {
    val listOfStudents: List[Student] = List(Student(1, "shivangi"), Student(2, "mahesh"),
      Student(2, "mahesh"))
    val listOfScoreCard = List(ScoreCard(1, Map(1L -> 1), 70), ScoreCard(2, Map(2L -> 2), 80))

    val studentNames = listOfStudents.map(x => x.name)
    if(studentNames.lengthCompare(unique(studentNames).size) == 0) {
      // No student has same name
      Map()
    } else {
      //student has same name

    }
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

  generateScore()
}
