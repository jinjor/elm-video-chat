
Elm.Native.WebSocket = {};

Elm.Native.WebSocket.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.WebSocket = localRuntime.Native.WebSocket || {};
    if (localRuntime.Native.WebSocket.values) return localRuntime.Native.WebSocket.values;

    var Task = Elm.Native.Task.make(localRuntime);
    var NS = Elm.Native.Signal.make(localRuntime);

    var message = NS.input('WebSocket.message', '');
    var opened = NS.input('WebSocket.opened', false);

    var connection = null;
    var connect = function(url) {
      return Task.asyncFunction(function(callback) {
        connection = new WebSocket(url, ['soap', 'xmpp']);
        connection.onopen = function() {
          localRuntime.notify(opened.id, true);
          callback(Task.succeed());
        };
        connection.onclose = function() {
          localRuntime.notify(opened.id, false);
        };
        connection.onmessage = function(e) {
          localRuntime.notify(message.id, e.data);
        };
      });
    };

    var send = function(s) {
      return Task.asyncFunction(function(callback) {
        console.log('send:' + s);
        if(connection) {
          connection.send(s);
          callback(Task.succeed());
        } else {
          callback(Task.fail());
        }
      });
    };

    return localRuntime.Native.WebSocket.values = {
      connect: connect,
      message: message,
      opened: opened,
      send: send
    };
};
