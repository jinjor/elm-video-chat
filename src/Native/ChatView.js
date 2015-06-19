
Elm.Native.ChatView = {};

Elm.Native.ChatView.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.ChatView = localRuntime.Native.ChatView || {};
    if (localRuntime.Native.ChatView.values) return localRuntime.Native.ChatView.values;

    var Task = Elm.Native.Task.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);

    var focus = function(id) {
      return Task.asyncFunction(function(callback) {
        //TODO
        localRuntime.setTimeout(function() {
          var el = document.getElementById(id)
          if(el) {
            var el = document.getElementById(id)
            el.focus();
            return callback(Task.succeed(Utils.Tuple0));
          } else {
            return callback(Task.fail("not found: " + id));
          }
        }, 10);
      });
    };
    var scrollDown = function(id) {
      return Task.asyncFunction(function(callback) {
        localRuntime.setTimeout(function() {
          var el = document.getElementById(id);
          if(el) {
              var scrollHeight = Math.max(el.scrollHeight, el.clientHeight);
              // el.scrollTop = scrollHeight - el.clientHeight;
              el.scrollTop = 9999;
            // return callback(Task.succeed(Utils.Tuple0));
          } else {
            // return callback(Task.fail("not found: " + id));
          }
        }, 30);//TODO
        return callback(Task.succeed(Utils.Tuple0));
      });

    }

    return localRuntime.Native.ChatView.values = {
      focus: focus,
      scrollDown: scrollDown
    };
};
