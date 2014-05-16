<!DOCTYPE html><html xmlns:fb="http://ogp.me/ns/fb#" itemtype="http://schema.org"><head><meta http-equiv="X-UA-Compatible" content="IE=Edge,chrome=IE7"><meta name="fragment" content="!"><meta name="robots" content="NOODP"><meta charset="utf-8"><meta property="og:title" content="Coursera"><meta property="og:type" content="website"><meta property="og:image" content="http://s3.amazonaws.com/coursera/media/Coursera_Computer_Narrow.png"><meta property="og:url" content="https://www.coursera.org/"><meta property="og:site_name" content="Coursera"><meta property="og:locale" content="en_US"><meta property="og:description" content="Take free online classes from 80+ top universities and organizations. Coursera is a social entrepreneurship company partnering with Stanford University, Yale University, Princeton University and others around the world to offer courses online for anyone to take, for free. We believe in connecting people to a great education so that anyone around the world can learn without limits."><meta property="fb:admins" content="727836538,4807654"><meta property="fb:app_id" content="274998519252278"><meta name="description" content="Take free online classes from 80+ top universities and organizations. Coursera is a social entrepreneurship company partnering with Stanford University, Yale University, Princeton University and others around the world to offer courses online for anyone to take, for free. We believe in connecting people to a great education so that anyone around the world can learn without limits."><meta name="image" content="http://s3.amazonaws.com/coursera/media/Coursera_Computer_Narrow.png"><meta name="apple-itunes-app" content="app-id=736535961"><title>Exploratory Data Analysis | Coursera</title>    <link href="https://dt5zaw6a98blc.cloudfront.net/site-static/3192817021c42077e06a18daece13e57504eba3f/css/spark.main.css" rel="stylesheet" type="text/css" >
      <link href="https://www.coursera.org/maestro/api/course/972082/course.css" rel="stylesheet" type="text/css" >
      <link href="https://dt5zaw6a98blc.cloudfront.net/site-static/3192817021c42077e06a18daece13e57504eba3f/css/hg.css" rel="stylesheet" type="text/css" >
    <link href="https://spark-public.s3.amazonaws.com/exdata/static/images/favicon.ico" rel="icon" >
</head><body><div id="fb-root"></div><div id="spark-help" class="hide"><div data-helpwidget data-helpwidget-link-help="http://support.coursera.org" data-helpwidget-link-discuss="http://class.coursera.org/mooc/" data-helpwidget-link-filter="coursera-admin-helpwidget-link" tabindex="0" role="button" aria-haspopup="true" aria-expanded="false" class="helpwidget-tab">Admin Help</div></div><a href="http://help.coursera.org/customer/widget/emails/new" target="_blank"><div style="z-index: 1" data-networkprioritysupportwidget role="button" class="helpwidget-tab hide">Premium Support</div></a><div id="spark" class="hide"><div id="main"></div>
<script type="text/javascript">
  window.Global = {
    userIsAdmin: false,
    userId: 3070434,

    // API base URL so that the Backbone knows what to call
    API_BASE_URL: 'https://class.coursera.org/exdata-002/human_grading/api/',

    COURSE_BASE_URL: '//class.coursera.org/exdata-002/',
    SITES_ASSETS_BASE_URL: 'https://dt5zaw6a98blc.cloudfront.net/site-static/3192817021c42077e06a18daece13e57504eba3f/',

    humanGradingLinkHref: "https:\/\/class.coursera.org\/exdata-002\/human_grading\/",
    humanGradingLinkText: "Peer Assessments",

    BACKBONE_BASE_URL: '//class.coursera.org/exdata-002/human_grading/view/',
    BACKBONE_ROOT: '/exdata-002/human_grading/view/',

    UPLOADABLE_BINARY_EXTENSIONS: [],
  };
</script>
</div><div id="origami"><div id="origami-load" style="position:absolute;top:0px;left:0px;width:100%;height:100%;background:#f5f5f5;padding-top:5%;"><div id="coursera-loading-nojs" style="text-align:center; margin-bottom:10px;display:none;">Please use a <a href="http://help.coursera.org/customer/portal/articles/1164387-do-i-need-any-special-software-or-computer-equipment-">modern browser </a> with JavaScript enabled to use Coursera.</div><div><span id="coursera-loading-js" style="display: none; padding-left:45%">loading &nbsp;&nbsp;<img src="https://d2wvvaown1ul17.cloudfront.net/site-static/images/icons/loading.gif"></span></div><noscript>div(style="text-align:center; margin-bottom:10px;")
  | Please use a 
  a(href="http://help.coursera.org/customer/portal/articles/1164387-do-i-need-any-special-software-or-computer-equipment-") modern browser 
  |  with JavaScript enabled to use Coursera.
  </noscript></div></div><!--[if gte IE 8]><script>document.getElementById("coursera-loading-js").style.display = 'block';</script><![endif]-->
<!--[if lte IE 7]><script>document.getElementById("coursera-loading-nojs").style.display = 'block';
window._204 = window._204 || [];
window._gaq = window._gaq || [];

window._gaq.push(
    ['_setAccount', 'UA-28377374-1'],
    ['_setDomainName', window.location.hostname],
    ['_setAllowLinker', true],
    ['_trackPageview', window.location.pathname]);
    
window._204.push(
  ['client', 'home'],
  {key:"pageview", value:window.location.pathname});
  </script><script src="https://eventing.coursera.org/204.min.js"></script><script src="https://ssl.google-analytics.com/ga.js"></script><![endif]-->
<!--[if !IE]> --><script>document.getElementById("coursera-loading-js").style.display = 'block';</script><!-- <![endif]-->
    <script src="https://dt5zaw6a98blc.cloudfront.net/site-static/3192817021c42077e06a18daece13e57504eba3f/pages/hg/thirdparty/webshims/extras/modernizr-custom.js" ></script>
  <script src="https://d2wvvaown1ul17.cloudfront.net/site-static/3192817021c42077e06a18daece13e57504eba3f/js/core/require.js"></script><script data-baseurl="https://d2wvvaown1ul17.cloudfront.net/site-static/3192817021c42077e06a18daece13e57504eba3f/" data-version="3192817021c42077e06a18daece13e57504eba3f" data-timestamp='1400034007930' data-debug='0' data-locale="en_US" id="_require">if(document.getElementById("coursera-loading-js").style.display == 'block') {
  (function(el) {
     require.config({
       //enforceDefine: true,
       waitSeconds: 75,
       baseUrl: el.getAttribute("data-baseurl"),
       urlArgs: el.getAttribute("data-debug") == "1" ? "v=" + el.getAttribute("data-timestamp") : "",
       shim: {
          'underscore': {
             exports: '_'
          },
          'backbone': {
             deps: ['underscore', 'jquery'],
             exports: 'Backbone',
             init: function(_, $) {
               Backbone.$ = $;
               return Backbone;
             }
          },
          'pages/hg/thirdparty/js/backbone.marionette': {
            deps: ["backbone"]
          },
          'pages/hg/thirdparty/js/webshims/polyfiller': {
            deps: ["jquery"]
          },
          'pages/hg/thirdparty/js/webshims/extras/mousepress': {
            deps: ["jquery", "pages/hg/thirdparty/js/webshims/polyfiller"]
          },
          'pages/hg/thirdparty/js/jquery.throttle': {
            deps: ["jquery"]
          },
          'js/lib/jquery.linkify': {
            deps: ["jquery"]
          },
          'pages/hg/thirdparty/js/js/bootstrap/alert': {
            deps: ["jquery", "pages/hg/thirdparty/js/bootstrap/transition"]
          },
          'spark/core/js/jquery_ui': {
            deps: ["jquery"]
          },
          'spark/core/js/jquery.history': {
            deps: ["jquery"]
          },
          'js/lib/bootstrap.tooltip': {
            deps: ["jquery"]
          },
          'pages/hg/thirdparty/js/bootstrap/transition': {
            deps: ["jquery"]
          },
          'pages/hg/thirdparty/js/bootstrap/tab': {
            deps: ["jquery"]
          },
          'bundles/videojs/lib/video.4.3.0': {
            exports: 'vjs'
          }
       },
       paths: {
          "jquery":                         "pages/spark/jquery",
          "underscore":                     "js/core/underscore.1.5.2",
          "backbone":                       "js/core/backbone.1.1.0",
          "backbone.relational":            "js/lib/backbone.relational.0.8.6",
          "i18n":                           "js/core/i18n._t",
          "pages/spark/models/user.json":   "empty:",
          "pages/spark/models/course.json": "empty:",
          "pages/spark/models/navbar.json": "empty:"
       },
       callback: function() {
                    require(["pages/hg/js/app/routes"]);
                },
       config: {
         i18n: {
           locale: (window.localStorage ? localStorage.getItem('locale') : '') || el.getAttribute('data-locale')
         }
       }
     });
  })(document.getElementById("_require"));
}
</script><script>define("pages/spark/models/user.json", [], function(){
      return JSON.parse("{\"id\":3070434,\"email_address\":\"martin.pernollet@gmail.com\",\"full_name\":\"Martin Pernollet\",\"locale\":\"en_US\",\"timezone\":\"Europe\\\/Paris\",\"access_group_id\":\"4\",\"registration_time\":\"1396875408\",\"last_access_time\":\"1400013725\",\"last_access_ip\":\"41.250.133.4\",\"signature_track_register_time\":\"0\",\"email_announcement\":\"1\",\"email_forum\":\"1\",\"in_signature_track\":\"0\",\"wishes_proctored_exam\":null,\"first_name\":\"Martin\",\"permissions\":[\"default\",\"allow_site_access\"],\"group\":\"Student\",\"anonymous\":false,\"forum_title\":\"Student\",\"signature_track_state\":2,\"verified_quizzes\":[],\"submitted_quizzes\":[\"97\",\"99\"],\"scheduled_proctored_exam\":null,\"last_chance_modal\":1}");
  });
define("pages/spark/models/course.json", [], function(){
      return JSON.parse("{\"id\":972082,\"isPrivate\":false,\"type\":\"public\",\"name\":\"Exploratory Data Analysis\",\"instructor\":\"Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD\",\"sessionName\":\"exdata-002\",\"externalBaseURL\":\"https:\\\/\\\/www.coursera.org\\\/\",\"shortname\":\"exdata\",\"host\":\"https:\\\/\\\/www.coursera.org\\\/\",\"assetUnversionedLink\":\"https:\\\/\\\/dt5zaw6a98blc.cloudfront.net\\\/site-static\\\/\",\"cheggID\":\"\",\"hasLTI\":true,\"badgevilleDomain\":\"\",\"linkable\":false,\"universityShortname\":\"jhu\",\"signatureTrackStatus\":{\"signature_track_enabled\":1,\"signature_track_duration_left\":\"5 days and 15 hours\",\"signature_track_duration_left_days\":\"5 days\",\"signature_track_sign_up_now\":1,\"signature_track_last_chance_dialog\":1},\"courseURLs\":{\"log_in_link\":\"https:\\\/\\\/accounts.coursera.org\\\/signin?post_redirect=https%3A%2F%2Faccounts.coursera.org%2Fsignin%3Fuser_action%3Dclass%26course_id%3D972082%26post_redirect%3Dhttps%253A%252F%252Fclass.coursera.org%252Fexdata-002%252Fauth%252Fauth_redirector%253Ftype%253Dlogin%2526subtype%253Dnormal%2526visiting%253D\",\"sign_up_link\":\"https:\\\/\\\/accounts.coursera.org\\\/signup?enroll_cid=972082&enroll_sn=exdata&enroll_n=Exploratory Data Analysis\",\"view_course_info_link\":\"https:\\\/\\\/www.coursera.org\\\/course\\\/exdata\",\"sign_up_link_cant_enroll\":\"https:\\\/\\\/accounts.coursera.org\\\/signup?post_redirect=https%3A%2F%2Fwww.coursera.org%2Fcourse%2Fexdata\"},\"courseBase\":\"https:\\\/\\\/class.coursera.org\\\/exdata-002\\\/\",\"search\":true,\"hideWiki\":true,\"hideMeetup\":false,\"aceStartRegistrationDate\":\"Thu  1 Jan 1970  1:00 AM CET\",\"aceEndRegistrationDate\":\"Thu  1 Jan 1970  1:00 AM CET\",\"aceExamStartDate\":\"Thu  1 Jan 1970  1:00 AM CET\",\"aceExamEndDate\":\"Thu  1 Jan 1970  1:00 AM CET\",\"aceExamRules\":\"\",\"aceExamDuration\":\"\",\"aceBeforeExam\":false,\"aceAfterExam\":true,\"aceHours\":null,\"aceCreditType\":\"\",\"acePrice\":null,\"aceExamID\":null,\"aceEligible\":false,\"canRegisterForACE\":false,\"specialization\":{\"numCourse\":9,\"image\":\"https:\\\/\\\/s3.amazonaws.com\\\/coursera\\\/specializations\\\/jhudatascience\\\/logo_small.png\",\"id\":1,\"short_name\":\"jhudatascience\",\"name\":\"Data Science\"},\"supportForums\":true,\"textbooks\":\"\",\"inVideoQuizV2\":false,\"in_flexjoin\":\"\",\"honorCodeExtraText\":\"\",\"honorCodeCustomTitle\":\"\",\"honorCodeCustomButton\":\"\"}");
  });
define("pages/spark/models/navbar.json", [], function(){
      return JSON.parse("{\"items\":[{\"name\":\"\",\"icon\":\"\",\"link_type\":\"circuit\",\"link_data\":\"\"},{\"name\":\"Course\",\"icon\":\"\",\"link_type\":\"heading\",\"link_data\":\"\"},{\"name\":\"Announcements\",\"icon\":\"home\",\"link_type\":\"circuit\",\"link_data\":\"class:index\"},{\"name\":\"Video Lectures\",\"icon\":\"lecture\",\"link_type\":\"circuit\",\"link_data\":\"lecture:index\"},{\"name\":\"Discussion Forums\",\"icon\":\"forum\",\"link_type\":\"circuit\",\"link_data\":\"forum:index\"},{\"name\":\"\",\"icon\":\"\",\"link_type\":\"circuit\",\"link_data\":\"\"},{\"name\":\"Exercises\",\"icon\":\"\",\"link_type\":\"heading\",\"link_data\":\"\"},{\"name\":\"Quizzes\",\"icon\":\"quiz\",\"link_type\":\"circuit\",\"link_data\":\"quiz:index\"},{\"name\":\"Course Projects\",\"icon\":\"assignment\",\"link_type\":\"circuit\",\"link_data\":\"human_grading:index\"},{\"name\":\"\",\"icon\":\"\",\"link_type\":\"circuit\",\"link_data\":\"\"},{\"name\":\"About the course\",\"icon\":\"\",\"link_type\":\"heading\",\"link_data\":\"\"},{\"name\":\"Syllabus\",\"icon\":\"wiki\",\"link_type\":\"wiki\",\"link_data\":\"syllabus\"},{\"name\":\"About the Instructor\",\"icon\":\"wiki\",\"link_type\":\"wiki\",\"link_data\":\"About_the_Instructor\"},{\"name\":\"\",\"icon\":\"\",\"link_type\":\"circuit\",\"link_data\":\"\"},{\"name\":\"Community\",\"icon\":\"\",\"link_type\":\"heading\",\"link_data\":\"\"}]}");
  });
</script></body></html>