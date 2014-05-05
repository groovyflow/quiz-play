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

object Application extends Controller {

  //AuthenticatedRequest and Authenticated are taken straight from http://www.playframework.com/documentation/2.2.x/ScalaActionsComposition
  class AuthenticatedRequest[A](val username: String, request: Request[A]) extends WrappedRequest[A](request)

  object Authenticated extends ActionBuilder[AuthenticatedRequest] {
    def invokeBlock[A](request: Request[A], block: (AuthenticatedRequest[A]) => Future[SimpleResult]) = {
      request.session.get("username").map { username =>
        block(new AuthenticatedRequest(username, request))
      } getOrElse {
        println("No username in session!")
        Future.successful(Forbidden)
      }
    }
  }

  def login = Action { request =>
    play.api.db.slick.DB.withSession { implicit session: play.api.db.slick.Session =>
      val usernameAndPassword: Option[(String, String)] = request.getQueryString("username").flatMap(userName =>
        request.getQueryString("password").map(password => (userName, password)))
      val userOption: Option[User] = usernameAndPassword.flatMap { case (username, password) => Users.findByUsernameAndPassword(username, password) }
      //By the way, you can only store a String in the Play Session. See http://www.playframework.com/documentation/2.0/ScalaSessionFlash 
      userOption.map { user: User => Ok(Json.obj("status" -> ("logged in " + user.username))).withSession { "username" -> user.username } }.getOrElse {
        Forbidden
      }
    }
  }

  implicit val questionReads: Reads[Question] = (
    (JsPath \ "id").read[Int] and
    (JsPath \ "text").read[String])(Question.apply _)

  implicit val choiceReads: Reads[Choice] = (
    (JsPath \ "id").read[Int] and
    (JsPath \ "text").read[String] and
    (JsPath \ "questionId").readNullable[Int] and
    (JsPath \ "next").readNullable[Int] and
    (JsPath \ "result").readNullable[Int])(Choice.apply _)

  //!!IMPORTANT!!  This combination reads must be after (that is, lower in the file) than the constituent question and choice Reads
  case class QuizReplyArg(question: Question, options: Seq[Choice], numChosen: Int)
  implicit val quizReplyArgReads: Reads[QuizReplyArg] = (
    (JsPath \ "prevQuestion" \ "question").read[Question] and
    (JsPath \ "prevQuestion" \ "options").read[Seq[Choice]] and
    (JsPath \ "answer").read[Int])(QuizReplyArg.apply _)

  def index = Action {
    println("index has been called.")
    Ok(views.html.index())
  }

  def quizReply = Authenticated(parse.json) { request: AuthenticatedRequest[JsValue] =>
    play.api.db.slick.DB.withSession { implicit session: play.api.db.slick.Session =>
      println("request.body is " + request.body)

      val quizReplyArgInJsResult: JsResult[QuizReplyArg] = (request.body).validate[QuizReplyArg]
      println("quizReplyArg is " + quizReplyArgInJsResult)
      quizReplyArgInJsResult.fold(
        invalid = { errors => BadRequest(JsError.toFlatJson(errors)) },
        valid = { quizReplyArg: QuizReplyArg =>
          //numOfChosen starts at 1 rather than 0, and that's why we need to subtract 1 to transform to a valid list index
          val chosen: Choice = quizReplyArg.options(quizReplyArg.numChosen - 1)

          //Get the username from the AuthenticatedRequest, which is a Request enriched by our Authenticated ActionBuilder.
          val userOption: Option[User] = Users.findByUsername(request.username)
          Answers.insert(Answer(None, quizReplyArg.question.id, chosen.id, userOption.get.id.get))
          chosen.nextQuestionId match {
            case Some(nextQuestionId) => //the choice has a next questionid, so let's find that question and its associated choices
              makeJsonForQuestionAndChoices(nextQuestionId).map(nextQuestion => Json.obj("data" -> nextQuestion, "status" -> "continue")) match {
                case None => BadRequest("no question for id " + nextQuestionId)
                case Some(jsonReplyContainingNextQuestionAndChoices) => Ok(jsonReplyContainingNextQuestionAndChoices)
              }
            case None => //There is no next question, so let's see if the quiz is over and we have a result to tell the user.
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

        })

    }
  }

  def findInitialQuestion = Authenticated { request: AuthenticatedRequest[AnyContent] =>
    println("findInitialQuestion called")
    play.api.db.slick.DB.withSession { implicit session: play.api.db.slick.Session =>
      Ok(Json.obj("data" -> Some(makeJsonForQuestionAndChoices(1)), "status" -> "continue"))
    }

  }

  def makeJsonForQuestionAndChoices(id: Int)(implicit s: play.api.db.slick.Session): Option[JsObject] = {
    val listOfQuestionsAndChoices = Questions.qAndC(id)
    listOfQuestionsAndChoices.headOption match {
      case None => None
      case Some(_) =>
        val choicesForThisQuestion = listOfQuestionsAndChoices.map { case (quest, choice) => Json.obj("id" -> Some(choice.id), "text" -> choice.text, "next" -> choice.nextQuestionId, "result" -> choice.resultId) }
        Some(Json.obj(
          "question" ->
            Json.obj("text" -> listOfQuestionsAndChoices.head._1.text, "id" -> Some(listOfQuestionsAndChoices.head._1.id)),
          "options" -> choicesForThisQuestion))
    }

  }

  def get = DBAction { implicit rs =>

    Ok(Json.obj("all Questions" -> Questions.findAll.map(row => (row.id.toString + " " + row.text))))
  }
}