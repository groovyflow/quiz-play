package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.db.slick._
import play.api.Play.current
import models._
import play.api.libs.json.JsPath
import play.api.libs.json.Json
import play.api.libs.json.{JsValue, JsArray, JsObject, JsUndefined, JsNull}
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

object Expt extends Controller{
  
  //We'll get rid of this when we figure out how to create seed data in evolutions.  Right now Slick somehow
  //overwrites our 1.sql file!!
  def seed = DBAction { implicit rs =>
    Questions.seed()
    Ok(Json.obj("id" -> "I think we seeded" ))
  }
  
  def index = Action {
    println("index has been called")
    Ok(views.html.index())
  }

  //TODO!! Getting pretty messy without using JSON readers and writers!! But remember, the shape of
  //the JSON changes depending on whether we need to send another question or a result.
  def quizReply = Action(parse.json) { request =>
    play.api.db.slick.DB.withSession { implicit session: play.api.db.slick.Session =>

      println("request.body is " + request.body)
      val answer = request.body \ "answer"
      val options = request.body \ "prevQuestion" \ "options"
      val userChoseThisOne = options.as[IndexedSeq[JsValue]].apply(jsValToInt(answer))
      val nextQuestionId = userChoseThisOne \ "next"
      Answers.insert(Answer(None, jsValToInt(request.body \ "prevQuestion" \ "question" \ "id"), jsValToInt(userChoseThisOne \ "id")))

      if (!isEmpty(nextQuestionId)) {
        findQAndChJson(jsValToInt(nextQuestionId)).map(nextQuestion => Json.obj("data" -> nextQuestion, "status" -> "continue")) match {
          case None => Ok("no question for id " + jsValToInt(nextQuestionId))
          case Some(x) => Ok(x)
        }
      } else {
        val resultId = userChoseThisOne \ "result"
        if (isEmpty(resultId)) BadRequest("No result for " + resultId)
        else {
          models.Results.find(jsValToInt(resultId)) match {
            case None => BadRequest("No result for " + resultId)
            case Some(result) =>
              Ok(Json.obj("status" -> "done", "result" -> Json.obj("text" -> result.text)))
          }
        }
      }
    }
  }
  
  //TODD  This should return an Option, probably by doing a Try in case of None.
  //Why do we need this? Because sometimes an Int is treated as a String rather than as a JsNumber, and
  //asOpt[JsInt] will throw an exception!  
  def jsValToInt(value: JsValue):Int = {
		  value.asOpt[Int] match {
		    case Some(x) => x
		    case None =>
		      value.as[String].toInt
		  }
  }

  def isEmpty(value: JsValue): Boolean = {
    value match {
      case _: JsUndefined => true
      case JsNull => true
      case _ => false
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
       //TDOO Need result and result table too!
       val options = listOfAnswersAndChoices.map{ case(quest, choice) =>  Json.obj("id" -> Some(choice.id), "text" -> choice.text, "next" -> choice.nextQuestionId, "result" -> choice.resultId ) }
        Some(Json.obj(
          "question" ->
            Json.obj("text" -> listOfAnswersAndChoices.head._1.text, "id" -> Some(listOfAnswersAndChoices.head._1.id)),
          "options" -> options))
   }   
   
 }
  
  def get = DBAction { implicit rs =>
    
    Ok(Json.obj("all Questions" -> Questions.findAll.map(row => (row.id.toString + " " + row.text)   )))
  }  
}