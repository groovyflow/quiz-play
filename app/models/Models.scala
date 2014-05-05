package models

import java.util.Date
import java.sql.{ Date => SqlDate }
import play.api.Play.current
import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.Tag
import java.sql.Timestamp

import scala.slick.lifted.Compiled


private[models] trait DAO {
  val QuestionsTableQuery = TableQuery[Questions]
  val ChoicesTableQuery = TableQuery[Choices]
  val ResultsTableQuery = TableQuery[Results]
  val AnswersTableQuery = TableQuery[Answers]
  val UsersTableQuery = TableQuery[Users]
}


case class Question(id: Int, text: String)
class Questions(tag: Tag) extends Table[Question](tag, "QUESTIONS") {
   def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
   def text: Column[String] = column[String]("TEXT")
   def * = (id, text) <> (Question.tupled, Question.unapply)
}

case class Choice(id: Int, text: String, questionId: Option[Int], nextQuestionId: Option[Int], resultId: Option[Int])
class Choices(tag: Tag) extends Table[Choice](tag, "Choices") {
   def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
   def text: Column[String] = column[String]("TEXT")
   def questionId = column[Option[Int]]("QUESTION_ID")
   def nextQuestionId = column[Option[Int]]("NEXT_QUESTION_ID")
   def resultId = column[Option[Int]]("RESULT_ID")
   def * =  (id, text, questionId, nextQuestionId, resultId) <> (Choice.tupled, Choice.unapply)
   def question= foreignKey("Quest_FK", questionId, TableQuery[Questions])(_.id)
}

case class Result(id: Option[Int], text: String)
class Results(tag: Tag) extends Table[Result](tag,"RESULTS") {
   def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
   def text: Column[String] = column[String]("TEXT") 
   def * =  (id.?, text) <> (Result.tupled, Result.unapply)
}

case class Answer(id: Option[Int], questionId: Int, choiceId: Int, userId: Int)
class Answers(tag: Tag) extends Table[Answer](tag, "ANSWERS") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def questionId = column[Int]("QUESTION_ID")
  def choiceId = column[Int]("CHOICE_ID")
  def userId = column[Int]("USER_ID")
  def * = (id.?, questionId, choiceId, userId) <> (Answer.tupled, Answer.unapply)
  def question = foreignKey("ANS_QUEST_FK", questionId, TableQuery[Questions])(_.id)
  def choice = foreignKey("ANS_CHOICE_FK", choiceId, TableQuery[Choices])(_.id)
}

case class User(id: Option[Int], username: String, password: String)
class Users(tag: Tag) extends Table[User](tag, "USERS") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def username: Column[String] = column[String]("USERNAME")
  def password: Column[String] = column[String]("PASSWORD")
  def * = (id.?, username, password) <>(User.tupled, User.unapply)
}

object Users extends DAO {
  def findByUsernameAndPassword(username: String, password: String)(implicit s: Session):Option[User] = {
    val query = for{
      user <- UsersTableQuery if user.username === username && user.password === password
    }yield(user)
    query.list.headOption
  }
  
  def findByUsername(username: String)(implicit s: Session):Option[User] = {
    val query = for{
      user <- UsersTableQuery if user.username === username 
    }yield(user)
    query.list.headOption
  }
}


object Questions extends DAO {
  def insert(question: Question)(implicit s: Session) = {
    QuestionsTableQuery.insert(question)
  }
  
  def insert(choice: Choice)(implicit s: Session) = {
    ChoicesTableQuery += choice
  }    
  
  def findAll()(implicit s: Session) = {
    val query = for{
      ques <- QuestionsTableQuery
    }yield(ques)
    println("questions query Statement is " + query.selectStatement)
    println("query result is " + query.list.map(row => row.text))
    query.list
  }  
  
  def seed()(implicit s: Session) = {
    if (Query(QuestionsTableQuery.length).first == 0) {
      ResultsTableQuery += Result(Some(1), "You are suited for the motor industry.")
      ResultsTableQuery += Result(Some(2), "You are ready to become a tycoon.")
      ResultsTableQuery += Result(Some(3), "Your future is in the financial sector.")
      ResultsTableQuery += Result(Some(4), "You are unlikely to find steady work.")
      QuestionsTableQuery += Question(1, "What kind of work environment do you prefer?")
      ChoicesTableQuery += Choice(1, "Intense.", questionId = Some(1), nextQuestionId = Some(2), None)
      ChoicesTableQuery += Choice(2, "Friendly.", questionId = Some(1), nextQuestionId = Some(3), None)


      QuestionsTableQuery += Question(2, "Please choose a favorite hobby from the options below:")
      ChoicesTableQuery += Choice(4, "Tinkering with cars.", questionId = Some(2), None, Some(1))
      ChoicesTableQuery += Choice(5, "Reading the newspaper.", questionId = Some(2), None, Some(2))

      QuestionsTableQuery += Question(3, "What is your favorite food?")
      ChoicesTableQuery += Choice(6, "Smoked salmon", questionId = Some(3), None, Some(3))
      ChoicesTableQuery += Choice(7, "Smoked cigarette butt", questionId = Some(3), None, Some(4))
      
      UsersTableQuery += User(None, "SomeUser", "SomePassword")
      println("find all returns " + findAll)
    }
  }
  

  
  def qAndC(id: Int)(implicit s: Session) = {
    val questionAndChoices = for {
      cho <- ChoicesTableQuery
      quest <- QuestionsTableQuery if quest.id === id && cho.questionId === quest.id//Had to use equals with three '='.  That is, '==='
    }yield(quest, cho)    
    val res = questionAndChoices.list
    println("result of qAndC query was " + res)
    res
  }
  
}

object Results extends DAO {
  def insert(result: Result)(implicit s: Session) = {
    ResultsTableQuery += result
  }  
  
   def find(id: Int)(implicit s: Session):Option[Result] = {
	   val res = ResultsTableQuery.where(_.id === id).firstOption
	   println("Results find retrieved " + res)
	   res
  }
}


object Answers extends DAO {
  def insert(answer: Answer)(implicit s: Session) = {
    println("persisting answer " + answer)
    AnswersTableQuery += answer   
  }
}