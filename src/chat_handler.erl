-module(chat_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
	 websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, []) ->
    case cowboy_http_req:header('Upgrade', Req) of
	{undefined, Req2} -> {ok, Req2, undefined};
	{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
	{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}],
				       %% HTML code taken and adepted from cowboy example file which took it from the  misultin's example file.
				       <<"
<html>
  <head>
    <script type='text/javascript'>
    var ws;
    function addMsg(text){
      var content = document.getElementById('content');
      content.innerHTML = content.innerHTML + text + '<br/>';
      content.lastChild.scrollIntoView();
    }
    function send(input, event) {
      if (event.keyCode == 13) {
        ws.send(document.getElementById('name').value + ': ' + input.value);
        input.value = '';
      }
    }
    function ready(){
      if ('MozWebSocket' in window) {
        WebSocket = MozWebSocket;
      }
      if ('WebSocket' in window) {
        // browser supports websockets
        ws = new WebSocket('ws://localhost:8080/');
        ws.onopen = function() {
          addMsg('websocket connected!');
        };
        ws.onmessage = function (evt) {
          var receivedMsg = evt.data;
          addMsg(receivedMsg);
        };
        ws.onclose = function() {
          // websocket was closed
          addMsg('websocket was closed');
        };
      } else {
        // browser does not support websockets
        addStatus('sorry, your browser does not support websockets.'); 
      }
    }
    </script>
  </head>
  <body onload='ready();'>
  <div id='content' style='overflow:scroll;height:90%'></div>
  <div id='input'>
    <input type='text' id='name'/>: 
    <input type='text' onkeyup='send(this, event);'/>
  </div>
  </body>
</html>">>,
				       Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    cowboy_chat_server:unregister(self()),
    ok.

websocket_init(_Any, Req, []) ->
    cowboy_chat_server:register(self()),
    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, undefined, hibernate}.

websocket_handle({text, Msg}, Req, State) ->
    cowboy_chat_server:msg(Msg),
    {ok, Req, State};

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({msg, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State, hibernate};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
