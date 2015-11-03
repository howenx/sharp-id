package controllers;

import play.Play;
import play.mvc.Controller;


/**
 * Created by china_005 on 15/10/23.
 */
public class SystemApplication extends Controller {
    public static final String STATIC_URL = Play.application().configuration().getString("staticUrl");

    public static final String INDEX_PAGE = Play.application().configuration().getString("indexPage");

    public static final String BILLING_PAGE = Play.application().configuration().getString("billingPage");




}


