package controllers;

import play.*;
import play.mvc.*;

import views.html.*;

/**
 *
 */

public class Application extends Controller {

    public static Result index() {
        return ok(index.render("Your new application is ready."));
    }


    public static Result login() {
        return ok("ok");
    }

    public static Result logout() {
        return ok("ok");
    }



}
