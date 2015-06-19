
Elm.Native.VideoControl = {};

Elm.Native.VideoControl.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.VideoControl = localRuntime.Native.VideoControl || {};
    if (localRuntime.Native.VideoControl.values) return localRuntime.Native.VideoControl.values;

    var Task = Elm.Native.Task.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);

    var requestFullScreen = function(src) {
      return Task.asyncFunction(function(callback) {
        var video = document.querySelector('video[src="' + src + '"]');
        if(video) {
          if (video.requestFullScreen) {
            video.requestFullScreen();
          } else if (video.mozRequestFullScreen) {
            video.mozRequestFullScreen();
          } else if (video.webkitRequestFullScreen) {
            video.webkitRequestFullScreen();
          }
        }
        return callback(Task.succeed(Utils.Tuple0));
      });
    };

    return localRuntime.Native.VideoControl.values = {
      requestFullScreen: requestFullScreen,
    };
};
