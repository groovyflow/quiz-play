package models

import java.util.Date
import java.sql.{ Date => SqlDate }
import play.api.Play.current
import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.Tag
import java.sql.Timestamp

import scala.slick.lifted.Compiled

case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

/**
 * Data Access Object trait
 *
 *  Used to create the DAOs: Companies and Computers
 */
private[models] trait DAO {
  val Companies = TableQuery[Companies]
  val Computers = TableQuery[Computers]
  val Questions = TableQuery[Questions]
  val Choices = TableQuery[Choices]
  val Results = TableQuery[Results]
  val Answers = TableQuery[Answers]
}

case class Company(id: Option[Long], name: String)

case class Computer(id: Option[Long] = None, name: String, introduced: Option[Date] = None, discontinued: Option[Date] = None, companyId: Option[Long] = None)

class Companies(tag: Tag) extends Table[Company](tag, "COMPANY") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.NotNull)
  def * = (id.?, name) <> (Company.tupled, Company.unapply _)
}

object Companies extends DAO {
  /**
   * Construct the Map[String,String] needed to fill a select options set
   */
  def options(implicit s: Session): Seq[(String, String)] = {
    val query = (for {
      company <- Companies
    } yield (company.id, company.name)).sortBy(_._2)
    query.list.map(row => (row._1.toString, row._2))
  }

  /**
   * Insert a new company
   * @param company
   */
  def insert(company: Company)(implicit s: Session) {
    Companies.insert(company)
  }
}

class Computers(tag: Tag) extends Table[Computer](tag, "COMPUTER") {

  implicit val dateColumnType = MappedColumnType.base[Date, Long](d => d.getTime, d => new Date(d))

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.NotNull)
  def introduced = column[Date]("introduced", O.Nullable)
  def discontinued = column[Date]("discontinued", O.Nullable)
  def companyId = column[Long]("companyId", O.Nullable)
  
  def * = (id.?, name, introduced.?, discontinued.?, companyId.?) <>(Computer.tupled, Computer.unapply _)
}

object Computers extends DAO {
  /**
   * Retrieve a computer from the id
   * @param id
   */
  def findById(id: Long)(implicit s: Session): Option[Computer] =
    Computers.where(_.id === id).firstOption

  /**
   * Count all computers
   */
  def count(implicit s: Session): Int =
    Query(Computers.length).first

  /**
   * Count computers with a filter
   * @param filter
   */
  def count(filter: String)(implicit s: Session): Int =
    Query(Computers.where(_.name.toLowerCase like filter.toLowerCase).length).first

  /**
   * Return a page of (Computer,Company)
   * @param page
   * @param pageSize
   * @param orderBy
   * @param filter
   */
  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%")(implicit s: Session): Page[(Computer, Option[Company])] = {

    val offset = pageSize * page
    val query =
      (for {
        (computer, company) <- Computers leftJoin Companies on (_.companyId === _.id)
        if computer.name.toLowerCase like filter.toLowerCase()
      } yield (computer, company.id.?, company.name.?))
        .drop(offset)
        .take(pageSize)

    val totalRows = count(filter)
    val result = query.list.map(row => (row._1, row._2.map(value => Company(Option(value), row._3.get))))

    Page(result, page, offset, totalRows)
  }

  /**
   * Insert a new computer
   * @param computer
   */
  def insert(computer: Computer)(implicit s: Session) {
    Computers.insert(computer)
  }

  /**
   * Update a computer
   * @param id
   * @param computer
   */
  def update(id: Long, computer: Computer)(implicit s: Session) {
    val computerToUpdate: Computer = computer.copy(Some(id))
    Computers.where(_.id === id).update(computerToUpdate)
  }

  /**
   * Delete a computer
   * @param id
   */
  def delete(id: Long)(implicit s: Session) {
    Computers.where(_.id === id).delete
  }
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
  
  //TODO Create seed data in evolutions.  But Slick plugin overwrote my 1.sql!!
  def seed()(implicit s: Session) = {
    Results += Result(Some(1), "Result A")
    Results += Result(Some(2), "Result B")
    Results += Result(Some(3), "Result C")
    Results += Result(Some(4), "Result D")    
    Questions += Question(1, "hello?")
    Choices += Choice(1, "a", questionId = Some(1), nextQuestionId = Some(2), None)
    Choices += Choice(2, "b", questionId = Some(1), nextQuestionId = Some(3), None)
    Choices += Choice(3, "c", questionId =None, nextQuestionId = None, None)
    
    Questions += Question(2, "2")
    Choices += Choice(4, "c", questionId =Some(2), None, Some(1))
    Choices += Choice(5, "d", questionId = Some(2), None, Some(2))
    
    Questions += Question(3, "3")
    Choices += Choice(6, "e", questionId = Some(3), None, Some(3))
    Choices += Choice(7, "f", questionId = Some(3), None, Some(4)) 
    println("find all returns " + findAll)
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