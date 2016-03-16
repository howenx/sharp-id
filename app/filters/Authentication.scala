package filters

/**
  * 用户token验证
  * Created by howen on 16/3/16.
  */
import play.api._
import play.api.mvc.Results._

import play.api.libs.iteratee._
import play.api.mvc.Security.AuthenticatedBuilder
import scala.concurrent.Future
import play.api.Logger
import play.api.Play.current
import play.api.cache.Cache
import play.api.mvc._
trait Authentication {

//  val AUTH_TOKEN_HEADER = "id-token"
//
      def username(request: RequestHeader) = request.session.get("email")
      def onUnauthorized(request: RequestHeader) = Results.Unauthorized("")
      def isAuthenticated(f: => String => Request[AnyContent] => Result) = {
        Security.Authenticated(username, onUnauthorized) { user =>
              Action(request => f(user)(request))
        }
//
//    def index = isAuthenticated { username => implicit request =>
//          Ok("Hello " + username)
//      }
//
//
//  def onUnauthorized(request: RequestHeader) = Results.Unauthorized

  // in a Security trait
  object Authenticated extends AuthenticatedBuilder(req => getUserFromRequest(req))

  // then in a controller
  def index = Authenticated { implicit request =>
    Ok("Hello " + request.user)
  }


  class AuthenticatedDbRequest[A](val user: User,
                                  val conn: Connection,
                                  request: Request[A]) extends WrappedRequest[A](request)

  object Authenticated extends ActionBuilder[AuthenticatedDbRequest] {
    def invokeBlock[A](request: Request[A], block: (AuthenticatedDbRequest[A]) => Future[Result]) = {
      AuthenticatedBuilder(req => getUserFromRequest(req)).authenticate(request, { authRequest: AuthenticatedRequest[A, User] =>
        DB.withConnection { conn =>
          block(new AuthenticatedDbRequest[A](authRequest.user, conn, request))
        }
      })
    }
  }

}

trait ScalaAuthenticatedController extends Controller with Authentication