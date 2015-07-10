
Elm.Native.WebSocket = {};

Elm.Native.WebSocket.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.WebSocket = localRuntime.Native.WebSocket || {};
    if (localRuntime.Native.WebSocket.values) return localRuntime.Native.WebSocket.values;

    var Task = Elm.Native.Task.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);
    var NS = Elm.Native.Signal.make(localRuntime);

    var message = NS.input('WebSocket.message', '');
    var opened = NS.input('WebSocket.opened', false);

    var connection = null;
    var interval = null;
    var connect = function(url) {
      return Task.asyncFunction(function(callback) {
        _connect(url, callback);
      });
    };
    var _connect = function(url, callback) {
      if(!connection) {
        try {
          connection = new WebSocket(url, ['soap', 'xmpp']);
          connection.onopen = function() {
            localRuntime.notify(opened.id, true);
            callback && callback(Task.succeed(Utils.Tuple0));
            if(interval) {
              clearInterval(interval);
              interval = null;
            }
          };
          connection.onclose = function() {
            localRuntime.notify(opened.id, false);
            if(!interval) {
              interval = setInterval(function() {
                console.log('trying reconnect...');
                _connect(url);
              }, 3000);
            }
            connection = null;
          };
          connection.onmessage = function(e) {
            localRuntime.notify(message.id, e.data);
          };
        } catch(e) {
          console.log(e);
          // localRuntime.notify(opened.id, false);
        }
      }
    };

    var send = function(s) {
      return Task.asyncFunction(function(callback) {
        // console.log('send:' + s);
        if(connection) {
          connection.send(s);
          return callback(Task.succeed(Utils.Tuple0));
        } else {
          return callback(Task.fail("send failed."));
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
