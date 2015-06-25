
package de.uni_luebeck.isp.tessla.wide

import akka.actor.ActorSystem
import spray.routing.SimpleRoutingApp

object ServerMain extends App with SimpleRoutingApp {
  implicit val system = ActorSystem()
  
  startServer(interface = "localhost", port = 8080) {
    path("") {
      get {
        complete {
          <html>
						<head>
							<title>Tessla Web IDE</title>
							<script type="text/javascript" src="static/tessla_wide-opt.js"></script>
							<script type="text/javascript" src="static/tessla_wide-jsdeps.min.js"></script>
							<script type="text/javascript" src="static/tessla_wide-launcher.js"></script>
							<link rel="stylesheet" type="text/css" href="/webjars/bootstrap/3.3.5/css/bootstrap.min.css"></link>
							<script type="text/javascript" src="/webjars/bootstrap/3.3.5/js/bootstrap.min.js"></script>
							<link rel="stylesheet" type="text/css" href="/webjars/codemirror/5.3/lib/codemirror.css"></link>
							<script type="text/javascript" src="/webjars/codemirror/5.3/lib/codemirror.js"></script>
							<script type="text/javascript" src="/webjars/codemirror/5.3/addon/mode/simple.js"></script>
							<script type="text/javascript" src="/webjars/codemirror/5.3/addon/hint/show-hint.js"></script>
							<link rel="stylesheet" type="text/css" href="/webjars/codemirror/5.3/addon/hint/show-hint.css"></link>
							<script type="text/javascript" src="/webjars/codemirror/5.3/addon/lint/lint.js"></script>
							<link rel="stylesheet" type="text/css" href="/webjars/codemirror/5.3/addon/lint/lint.css"></link>
							<script type="text/javascript" src="/webjars/codemirror/5.3/mode/clike/clike.js"></script>
							<script type="text/javascript" src="/webjars/codemirror/5.3/mode/xml/xml.js"></script>
						</head>
						<body>
    					<div class="container">
								<h1>Tessla Web IDE</h1>
								<p>Loading JavaScript, stand by…</p>
							</div>
						</body>
					</html>
        }
      }
    } ~
    pathPrefix("static") {
      getFromResourceDirectory("de/uni_luebeck/isp/tessla/wide/static")
    } ~
    pathPrefix("webjars") {
      getFromResourceDirectory("META-INF/resources/webjars/")
    }
  }
}