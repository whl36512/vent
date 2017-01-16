package controllers

import javax.inject._

import play.api._
import play.api.mvc._
import play.api.cache._
import play.api.cache.Cached // for page cache
import play.api.i18n._

//import play.api.db._
import play.api.data._
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.i18n.DefaultMessagesApi
import play.api.i18n.DefaultLangs
import play.api.libs.ws.WSClient
import models._
import play.api.db.Database
import scala.concurrent.duration._
import play.api.Logger

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class Application @Inject() (implicit val cached: Cached, implicit val cache: CacheApi, implicit val messagesApi: MessagesApi, implicit val db: Database, implicit val ws: WSClient)
    extends Controller with I18nSupport {

  /**
   * Create an Action to render an HTML page with a welcome message.
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */

  //private val topic = Topic("")

  def todo = TODO

  def echo = Action { reques =>
    Ok("Got request [" + reques + "]")
  }

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def credit = Action {
    Ok(views.html.credit())
  }

  def about = Action {
    Ok(views.html.about())
  }
  
  def terms = Action {
    Ok(views.html.terms())
  }
  

  def topicList = {
    val caching = cached.status({ r: RequestHeader => r.uri }, 200, 10).includeStatus(404, 100)
    caching {
      Action { implicit request =>
        Ok(views.html.topiclist(db))
      }
    }
  }

  //  def newTopic = Action { implicit request =>
  //    {
  //      //put request fields into form.data
  //      val fields =request.queryString.map{case (k,v) => (k,v(0))}
  //      val form = Link.form.bind(fields) // binding will fail but we get the data into the form.data
  //      Ok(views.html.newtopicform(form))
  //    }
  //  }
  //
  //  def newTopicPost = Action { implicit request =>
  //    Link.form.bindFromRequest.fold(
  //      formWithErrors => {
  //        // binding failure, you retrieve the form containing errors:
  //        BadRequest(views.html.newtopicform(formWithErrors))
  //      },
  //      link => {
  //        /* binding success, you get the actual value. */
  //        /* flashing uses a short lived cookie */
  //        Ok("Result: " + link.saveTo(db))
  //        //Redirect(routes.Application.newTopic()).flashing("success" -> ("Successful " + topic.toString))
  //      })
  //  }

  def saveTopic = Action { implicit request =>
    val queryString = request.body.asFormUrlEncoded.get.map { case (k, v) => (k, v(0)) } // get form data from POST

    val link = Link(topicId = queryString("topicId"), url = queryString("url"), publishTS = queryString("publishTS"), author = queryString("author"), title = queryString("title"))
    Ok(link.saveTo(db)(0).get("result").getOrElse("ERROR 20161231130101: Unknown error"))
    //    Ok(link.saveTo(db)(0).toString)
    //Redirect(routes.Application.newTopic()).flashing("success" -> ("Successful " + topic.toString))
  }

  def comments = {
    val caching = cached.status({ r: RequestHeader => r.uri }, 200, 20).includeStatus(404, 100)
    caching {
      Action { implicit request =>
        val queryString = request.queryString.map { case (k, v) => (k, v(0)) } // one key has only one value        
        Ok(views.html.comments(db, queryString.get("linkId").getOrElse(""), queryString.get("topicId").getOrElse("")))
      }
    }
  }

  //  def comment = Action { implicit request =>
  //    {
  //      val fields = parseRequest(request, Array("linkId", "parentMsgId"))
  //      val queryString = request.queryString.map { case (k, v) => (k, v(0)) } // linkId or paraentMsgId should be in the Map
  //      //      Ok(queryString.toString)
  //      //Ok(views.html.comment(Msg.form)).withSession(new Session(queryString))
  //      Ok(views.html.comment(Msg.form)).withSession(new Session(queryString))
  //      
  //    }
  //  }
  //
  //  def commentPost = Action { implicit request =>
  //    Msg.form.bindFromRequest.fold(
  //      formWithErrors => {
  //        //      val fields = parseRequest(request, Array("linkId", "parentMsgId"))
  //        //Ok(formWithErrors.toString)
  //        // binding failure, you retrieve the form containing errors:
  //        //        BadRequest(views.html.comment(formWithErrors, fields("linkId"), fields("parentMsgId")))
  //        BadRequest(views.html.comment(formWithErrors))
  //      },
  //      msg => { // msg has only content field populated. linkId and parentMsgId are in the session data
  //        /* binding success, you get the actual value. */
  //        /* flashing uses a short lived cookie */
  //        //Ok("this is the request ->" + msg.toString)
  //
  //        //Ok("this is the url return ->" + msg.saveTo(db))
  //        // id is either linkId or parentMsgId
  //        val form = Msg.form.bindFromRequest()
  //        // either linkId or parentMsgId is empty
  //        val linkId = request.session.get("linkId").getOrElse[String]("")
  //        val parentMsgId = request.session.get("parentMsgId").getOrElse[String]("")
  //        linkId + parentMsgId match {
  //          case "" => BadRequest(views.html.comment(form))
  //          case id => {
  //            val msg2 = msg.copy(linkId = linkId, parentMsgId = parentMsgId)
  //            val idInCache = "addcomment:" + linkId + parentMsgId
  //            cache.set(idInCache, msg2) // save ids and content to cache
  //            Ok(views.html.commentconfirm(form))
  //          }
  //        }
  //
  //        //Redirect(routes.Application.newTopic()).flashing("success" -> ("Successful " + topic.toString))
  //      })
  //  }
  def saveComment = Action { implicit request =>
    val ipHash = Msg.hashIp(request.remoteAddress)
    val userThrottle = cache.get[UserThrottle](ipHash).getOrElse(UserThrottle())
    if (userThrottle.commentThrottle.isBlocked) {
      BadRequest("ERROR 20161229134526: Comment blocked. Reason: Too frequently")
    } else {
      val queryString = request.body.asFormUrlEncoded.get.map { case (k, v) => (k, v(0)) } // get form data from POST

      //Ok("queryString="+ queryString.toString)
      val Array(content, linkId, msgId) = Array("content", "linkId", "msgId").map { x => queryString.get(x).getOrElse("") }

      val msg = Msg(linkId = linkId, parentMsgId = msgId, content = content, ipHash = ipHash)
      msg.validate match {
        case "" => {  // no error
          val result = msg.saveTo(db)(0)("cnt")
          cache.set(ipHash, userThrottle, 60.minutes)
          Ok("Result: " + result + """ comment saved. """)
        }
        case error => BadRequest(error)
      }
    }
  }

  def commentConfirmPost = Action { implicit request =>
    val linkId = request.session.get("linkId").getOrElse[String]("")
    val parentMsgId = request.session.get("parentMsgId").getOrElse[String]("")
    val idInCache = "addcomment:" + linkId + parentMsgId
    val msg = cache.get[Msg](idInCache).getOrElse(null)
    msg match {
      case null => Ok("system ERROR 20161224: msg not found in cache")
      case msg => {
        cache.remove(idInCache) // clear cache to prevent double posting
        Ok("Result: " + msg.saveTo(db)(0)("cnt") + """ comment saved. """)
      }
      //Redirect(routes.Application.newTopic()).flashing("success" -> ("Successful " + topic.toString))
    }
  }

  def flag = Action { implicit request =>
    val ipHash = Msg.hashIp(request.remoteAddress)
    val userThrottle = cache.get[UserThrottle](ipHash).getOrElse(UserThrottle())
    if (userThrottle.flagThrottle.isBlocked) {
      Ok("ERROR 20161229143514: Flagging blocked. Reason: Too frequently")
    } else {
      val msgId = request.getQueryString("msgId").getOrElse("")
      val result = Msg(msgId = msgId).flag(db)
      cache.set(ipHash, userThrottle, 60.minutes) // set cache after getting result
      Ok("Result: " + result)
    }
  }

  def parseRequest(r: Request[AnyContent], fields: Array[String]) = {
    var fieldMap = Map[String, String]()
    for (key <- fields) {
      val vOpt = r.getQueryString(key)
      vOpt match {
        case Some(v) => fieldMap += (key -> v)
        case None    => fieldMap += (key -> "")
      }
    }
    fieldMap
  }
}




