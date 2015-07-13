
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
    var pingInterval = null;
    var pingFlag = false;
    var connect = function(url) {
      return Task.asyncFunction(function(callback) {
        _connect(url, callback);
      });
    };
    var _connect = function(url, callback) {
      if(!connection) {
        try {
          connection = new WebSocket(url, ['soap', 'xmpp']);
          var tryStartReconnecting = function() {
            if(!interval) {
              interval = setInterval(function() {
                console.log('trying reconnect...');
                _connect(url);
              }, 3000);
            }
          };
          var stopPing = function() {
            if(pingInterval) {
              clearInterval(pingInterval);
              pingInterval = null;
            }
          };

          connection.onopen = function() {
            localRuntime.notify(opened.id, true);
            callback && callback(Task.succeed(Utils.Tuple0));
            if(interval) {
              clearInterval(interval);
              interval = null;
            }
            pingInterval = setInterval(function() {
              // console.log('ping');
              connection.send('Ping');
              pingFlag = true;
              setTimeout(function() {
                if(pingFlag) {
                  pingFlag = false;
                  stopPing();
                  connection = null;
                  tryStartReconnecting();
                }
              }, 3 * 1000);
            }, 1 * 10 * 1000);
          };
          connection.onclose = function() {
            localRuntime.notify(opened.id, false);
            stopPing();
            connection = null;
            tryStartReconnecting();
          };
          connection.onmessage = function(e) {
            if(e.data === 'Pong') {
              // console.log('pong');
              pingFlag = false;
            } else {
              localRuntime.notify(message.id, e.data);
            }
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
