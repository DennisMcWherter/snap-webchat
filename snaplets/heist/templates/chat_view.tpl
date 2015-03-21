<bind tag="header-css">
<style type="text/css">
#chatBox {
  height: 300px;
  width: auto;
  overflow: scroll;
  border: 1px solid #000;
}
#messageBox * {
  width: 100%;
}
</style>
</bind>

<bind tag="footer-scripts">
<script type="text/javascript">
  // NOTE: This is 2015... Everything should be secure (i.e. wss).
  //   My only excuse is that this is a tutorial :)
  var conn = new WebSocket("ws://localhost:8000/chat/chat");
  conn.onopen = function() {
    console.log("Connection opened...");
  }
  conn.onclose = function() {
    console.log("Connection to chat closed.");
  }
  conn.onerror = function(e) {
    console.log("Error detected: " + e);
  }
  conn.onmessage = function(msg) {
    document.getElementById('chatBox').innerHTML += msg.data + "<br>";
  }

  // Pure JS. Should really use jquery or some lib
  document.getElementById('messageBox').onkeypress = function(e) {
    if (!e) e = window.event;
    var keyCode = e.keyCode || e.which;
    if (keyCode == '13') {
      sendMessage();
      return false;
    }
  }

  function sendMessage() {
    var msg = document.getElementById('chatMessage');
    if (msg.value.length > 0) {
      conn.send(msg.value);
    }
    msg.value = "";
  }
</script>
</bind>

<apply template="base">
<div id="chat">
  <div id="chatBox">
  </div>
  <div id="messageBox">
    <input type="text" id="chatMessage" onClick="javascript:sendMessage();" />
  </div>
</div>
</apply>
