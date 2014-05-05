import java.text.SimpleDateFormat
import play.api._
import models._
import play.api.db.slick._
import play.api.Play.current

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    InitialData.insert()
  }

}

/**
 * Initial set of data to be imported
 */
object InitialData {
  def insert() {
    println("inserting on startup")
    DB.withSession { implicit s: Session =>
      Questions.seed()
    }
  }
}