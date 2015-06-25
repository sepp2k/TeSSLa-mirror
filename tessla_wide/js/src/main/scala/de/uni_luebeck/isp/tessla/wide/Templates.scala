package de.uni_luebeck.isp.tessla.wide

object Templates {
  val navigation = """
<nav class="navbar navbar-inverse navbar-fixed-top">
  <div class="container-fluid">
    <div class="navbar-header">
      <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
        <span class="sr-only">Toggle navigation</span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
      <a class="navbar-brand" href="#">Tessla Web IDE</a>
    </div>
    <div id="navbar" class="navbar-collapse collapse">
      <ul class="nav navbar-nav navbar-right">
        <li class="dropdown">
          <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Examples <span class="caret"></span></a>
          <ul class="dropdown-menu" id="example-list">
          </ul>
        </li>
      </ul>
    </div>
  </div>
</nav>
<style>body { padding-top: 70px; }</style>
"""
  val editors = """
    
<style>
  .CodeMirror {border: 1px solid #888; height: calc(80vh - 100px); }
  .CodeMirror-focused {border: 1px solid #f00; height: calc(80vh - 100px); }
  .cm-operator {color: #00a; }
  .tessla-error {
    background: #faa;
  }
  .CodeMirror-lint-tooltip {
    z-index: 1100;
  }
  #parse-result {
    font-family: monospace;
  }
</style>
<div class="container-fluid">
  <div class="row">
    <div class="col-md-6"><textarea id="tessla-editor"></textarea></div>
    <div class="col-md-6"><textarea id="target-editor"></textarea></div>
  </div>
  <div class="row">
    <div class="col-md-12" id="parse-result">
    </div>
  </div>
</div>
  
"""
}