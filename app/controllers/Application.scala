package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.db.slick._
import play.api.Play.current
import models._
import play.api.libs.json.JsPath
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._ 
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import views.html.defaultpages.badRequest

//TODO  Async  I think this means I need a Promise[whateverTheSlickDAOReturns]
//And can't use the DbAction, because it doesn't have async.  Instead:
/*
   def create = Action.async {
    //Probably don't need database access here!
    play.api.db.slick.DB.withSession { implicit session: play.api.db.slick.Session =>
      Future {
        ....
      }
    }
  }
 */

object Application extends Controller{
  
  //http://www.playframework.com/documentation/2.2.x/ScalaJsonCombinators 
  
  implicit val questionReads: Reads[ Question] = (
      (JsPath \ "id").read[Int] and
      (JsPath \ "text") .read[String]
      )(Question.apply _)
      
      
   implicit val choiceReads: Reads[Choice] = (
       (JsPath \ "id").read[Int] and
       (JsPath \ "text").read[String] and
       (JsPath \ "questionId").readNullable[Int] and
       (JsPath \ "next").readNullable[Int] and
       (JsPath \ "result").readNullable[Int]
       )(Choice.apply _)
       
 //!!IMPORTANT!!  This combination reads must be defined after its constituent reads
 case class QuizReplyArg(question: Question, options: Seq[Choice], numChosen: Int)  
  implicit val quizReplyArgReads: Reads[QuizReplyArg] = (
    (JsPath \ "prevQuestion" \ "question").read[Question] and
    (JsPath \ "prevQuestion" \ "options").read[Seq[Choice]] and
    (JsPath \ "answer").read[Int]
  )(QuizReplyArg.apply _)       
  

  def index = Action {
    println("index has been called.")
    Ok(views.html.index())
  }

  def quizReply = Action(parse.json) { request =>
    play.api.db.slick.DB.withSession { implicit session: play.api.db.slick.Session =>
      println("request.body is " + request.body)

      val quizReplyArg = (request.body).validate[QuizReplyArg]
      println("quizReplyArg is " + quizReplyArg)
      quizReplyArg.fold(
        invalid = { errors => BadRequest(JsError.toFlatJson(errors)) },
        valid = { quizReplyArg: QuizReplyArg =>
          //numOfChosen starts at 1 rather than 0, and that's why we need to subtract 1 to transform to a valid list index
          val chosen: Choice = quizReplyArg.options(quizReplyArg.numChosen - 1)
          Answers.insert(Answer(None, quizReplyArg.question.id, chosen.id))
          chosen.nextQuestionId match {
            case Some(nextQuestionId) => //the choice has a next questionid, so let's find that question and its assocatied choices
              findQAndChJson(nextQuestionId).map(nextQuestion => Json.obj("data" -> nextQuestion, "status" -> "continue")) match {
                case None => BadRequest("no question for id " + nextQuestionId)
                case Some(jsonReplyContainingNextQuestionAndChoices) => Ok(jsonReplyContainingNextQuestionAndChoices)
              }
            case None =>  //There is no next question, so let's see if the quiz is over and we have a result to tell the user.
              chosen.resultId match {
                case None => BadRequest("No nextQuestion and no result on chosen answer")
                case Some(resultId) =>
                  models.Results.find(resultId) match {
                    case None => BadRequest("No Result domain object for " + resultId)
                    case Some(result) =>
                      Ok(Json.obj("status" -> "done", "result" -> Json.obj("text" -> result.text)))
                  }
              }
          }

        }
      )

    }
  }
  

  def quizFirst = Action { request =>
    println("quizFirst called")
    play.api.db.slick.DB.withSession { implicit session: play.api.db.slick.Session =>
      Ok(Json.obj("data" -> Some(findQAndChJson(1)), "status" -> "continue"))
    }

  }
  
  def create = DBAction { implicit rs =>
    Ok(Json.obj("id" -> Questions.insert(Question(3, text="whee"))))
  }
  
  //TODO  Maybe get rid of this. Or change it back so that it doesn't have the added "data" and "status" keys, which
  //were merely a practice for quizReply
 def find(id: Int) = DBAction { implicit rs =>
   println("find has been called")
   //Should do option map here!! 
   val listOfAnswersAndChoices = Questions.qAndC(id)
   listOfAnswersAndChoices.headOption match {
     case None => InternalServerError("no question for id = " + id)
     case Some(_) => 
       //TDOO Need result and result table too!
       val options = listOfAnswersAndChoices.map{ case(quest, choice) =>  Json.obj("text" -> choice.text, "next" -> choice.nextQuestionId) }
      // Ok(Json.obj("question" -> listOfAnswersAndChoices.head._1.text, "options" -> options ))
       println("what the fuck?")
       println("findQAndCh in Expt::find " +    findQAndChJson(id))
       println("what we want " + Json.obj("data" -> findQAndChJson(id).toString, "status" -> "continue"))
       //Ok(findQAndCh(id))
       //Ok(Json.obj("question" -> listOfAnswersAndChoices.head._1.text, "options" -> options ))
       Ok(Json.obj("data" -> Some(findQAndChJson(id)), "status" -> "continue"))
   }
  }
 
 def findQAndChJson(id: Int)(implicit s: play.api.db.slick.Session): Option[JsObject] = {
   val listOfAnswersAndChoices = Questions.qAndC(id)
   listOfAnswersAndChoices.headOption match {
     case None => None
     case Some(_) => 
       val choicesForThisQuestion = listOfAnswersAndChoices.map{ case(quest, choice) =>  Json.obj("id" -> Some(choice.id), "text" -> choice.text, "next" -> choice.nextQuestionId, "result" -> choice.resultId ) }
        Some(Json.obj(
          "question" ->
            Json.obj("text" -> listOfAnswersAndChoices.head._1.text, "id" -> Some(listOfAnswersAndChoices.head._1.id)),
          "options" -> choicesForThisQuestion))
   }   
   
 }
  
  def get = DBAction { implicit rs =>
    
    Ok(Json.obj("all Questions" -> Questions.findAll.map(row => (row.id.toString + " " + row.text)   )))
  }  
}