package models

import java.util.Date
import java.sql.{ Date => SqlDate }
import play.api.Play.current
import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.Tag
import java.sql.Timestamp

import scala.slick.lifted.Compiled


private[models] trait DAO {
  val Questions = TableQuery[Questions]
  val Choices = TableQuery[Choices]
  val Results = TableQuery[Results]
  val Answers = TableQuery[Answers]
}


case class Question(id: Int, text: String)
class Questions(tag: Tag) extends Table[Question](tag, "QUESTIONS") {
   def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
   def text: Column[String] = column[String]("TEXT")
   //def * : ProvenShape[(Int, String)] = (id, text) //Why not ProvenShape[Quesion] or ProvenShape[Question.unapply] or something?
   def * = (id, text) <> (Question.tupled, Question.unapply)
}

case class Choice(id: Int, text: String, questionId: Option[Int], nextQuestionId: Option[Int], resultId: Option[Int])
//TODO  I saw examples that didn't seem to use Column[Option[<Primitve-Type>]], but gettring rid of that here caused
//a "NULL Not allowed for column ... error".  And this even happened when I had my nextQuestionId.? syntax in the 'def *' function
class Choices(tag: Tag) extends Table[Choice](tag, "Choices") {
   def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
   def text: Column[String] = column[String]("TEXT")
   def questionId = column[Option[Int]]("QUESTION_ID")
   //TODO Don't know how to make this a nullable foreign key!!
   def nextQuestionId = column[Option[Int]]("NEXT_QUESTION_ID")
   def resultId = column[Option[Int]]("RESULT_ID")
   def * =  (id, text, questionId, nextQuestionId, resultId) <> (Choice.tupled, Choice.unapply)
   //def * : ProvenShape[(Int, String, Int)] = (id, text, questionId)
   def question= foreignKey("Quest_FK", questionId, TableQuery[Questions])(_.id)
   //def nextQuestion= foreignKey("NQuest_FK", nextQuestionId, TableQuery[Questions])(_.id)
}

case class Result(id: Option[Int], text: String)
class Results(tag: Tag) extends Table[Result](tag,"RESULTS") {
   def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
   def text: Column[String] = column[String]("TEXT") 
   def * =  (id.?, text) <> (Result.tupled, Result.unapply)
}

case class Answer(id: Option[Int], questionId: Int, choiceId: Int)
class Answers(tag: Tag) extends Table[Answer](tag, "ANSWERS") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def questionId = column[Int]("QUESTION_ID")
  def choiceId = column[Int]("CHOICE_ID")
  def * = (id.?, questionId, choiceId) <> (Answer.tupled, Answer.unapply)
  def question = foreignKey("ANS_QUEST_FK", questionId, TableQuery[Questions])(_.id)
  def choice = foreignKey("ANS_CHOICE_FK", choiceId, TableQuery[Choices])(_.id)
}

object Questions extends DAO {
  def insert(question: Question)(implicit s: Session) = {
    Questions.insert(question)
  }
  
  def findAll()(implicit s: Session) = {
    //TODO Compiled statements
    val query = for{
      ques <- Questions
    }yield(ques)
    println("questions query Statement is " + query.selectStatement)
    println("query result is " + query.list.map(row => row.text))
    //query.list.map(row => row.text)
    query.list
    //Questions.insert(question)
  }  
  
  //TODO Put this logic in Global::insertData. But make sure all our DAOs have insert methods
  def seed()(implicit s: Session) = {
    if (Query(Questions.length).first == 0) {
      Results += Result(Some(1), "Result A")
      Results += Result(Some(2), "Result B")
      Results += Result(Some(3), "Result C")
      Results += Result(Some(4), "Result D")
      Questions += Question(1, "hello?")
      Choices += Choice(1, "a", questionId = Some(1), nextQuestionId = Some(2), None)
      Choices += Choice(2, "b", questionId = Some(1), nextQuestionId = Some(3), None)
      Choices += Choice(3, "c", questionId = None, nextQuestionId = None, None)

      Questions += Question(2, "2")
      Choices += Choice(4, "c", questionId = Some(2), None, Some(1))
      Choices += Choice(5, "d", questionId = Some(2), None, Some(2))

      Questions += Question(3, "3")
      Choices += Choice(6, "e", questionId = Some(3), None, Some(3))
      Choices += Choice(7, "f", questionId = Some(3), None, Some(4))
      println("find all returns " + findAll)
    }
  }
  

  
  def qAndC(id: Int)(implicit s: Session) = {
    val questionAndChoices = for {
      cho <- Choices
      quest <- Questions if quest.id === id && cho.questionId === quest.id//Had to use equals with three '='.  That is, '==='
    }yield(quest, cho)    
    val res = questionAndChoices.list
    println("result of qAndC query was " + res)
    res
  }
  
  //Different synax, same result, and this one depends on having defined a foreign key, so we're not using it.
/*  def qAndC(id: Int)(implicit s: Session) = {
    val questionAndChoices = for {
      cho <- Choices
      quest <- cho.question if quest.id === id //Had to use equals with three '='.  That is, '==='
    }yield(quest, cho)    
    questionAndChoices.list
  }  */
  
  //Todo Trhied to compile, but got this error: could not find implicit value for parameter s: play.api.db.slick.Config.driver.simple.Session
  //Looks like I need to compile inside a session.  How would that work?
  //val qAndCCompiled = Compiled(q_c _)
  


/*
    val questionAndOptions = for {
      opt <- options
      quest <- questions if quest.id === 1 && opt.questionId === quest.id//Had to use equals with three '='.  That is, '==='
    }yield(opt, quest)
 */  
  
  
}

object Results extends DAO {
   def find(id: Int)(implicit s: Session):Option[Result] = {
	   val res = Results.where(_.id === id).firstOption
	   println("Results find retrieved " + res)
	   res
  }
}


object Answers extends DAO {
  def insert(answer: Answer)(implicit s: Session) = {
    println("persisting answer " + answer)
    Answers += answer   
  }
}