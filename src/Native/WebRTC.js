(function() {

  function Tuple3(x,y,z) {
		return {
			ctor: '_Tuple3',
			_0: x,
			_1: y,
      _2: z
		};
	}

  Elm.Native.WebRTC = {};

  Elm.Native.WebRTC.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.WebRTC = localRuntime.Native.WebRTC || {};
    if (localRuntime.Native.WebRTC.values) return localRuntime.Native.WebRTC.values;

    var Task = Elm.Native.Task.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);
    var NS = Elm.Native.Signal.make(localRuntime);
    var requests = NS.input('WebRTC.requests', Tuple3('', '', ''));
    var _onLocalVideoURL = NS.input('WebRTC.onLocalVideoURL', Utils.Tuple2('', ''));
    var _onRemoteVideoURL = NS.input('WebRTC.onRemoteVideoURL', Utils.Tuple2(Tuple3('', '', 0), ''));
    var _onAddConnection = NS.input('WebRTC.onAddConnection',  Tuple3('', '', 0));
    var _onRemoveConnection = NS.input('WebRTC.onRemoveConnection',  Tuple3('', '', 0));

    //TODO
    var cm = null;
    var clientId = null;
    var room = getRoom();//TODO

    var _initialize = function(_clientId, iceServers) {
      iceServers = listToArray(iceServers);
      clientId = _clientId;
      cm = createConnectionManager({
        iceServers: iceServers
      }, send);
      return Task.asyncFunction(function(callback) {
        callback(Task.succeed(Utils.Tuple0));
      });
    };
    var send = function(data) {
      data.room = room;
      data.from = clientId;
      console.log('RTC send: ' + data.type);
      setTimeout(function() {
        localRuntime.notify(requests.id, Tuple3(data.type, data.to || '', data.data));
      });
    };

    var _answerSDP = function(from, data) {
      return Task.asyncFunction(function(callback) {
        answerSDP(clientId, cm, send, {
          from: from,
          data: data
        }, function onRemoteVideoURL(from, mediaType, url, upstream) {
          setTimeout(function() {
            localRuntime.notify(_onRemoteVideoURL.id, Utils.Tuple2(Tuple3(from, mediaType, upstream?1:0), url));
          });
        }, function onAddConnection(from, mediaType, upstream) {
          setTimeout(function() {
            localRuntime.notify(_onAddConnection.id, Tuple3(from, mediaType, upstream?1:0));
          });
        }, function onRemoveConnection(from, mediaType) {
          setTimeout(function() {
            localRuntime.notify(_onRemoveConnection.id, Tuple3(from, mediaType, upstream?1:0));
          });
        });
        callback(Task.succeed(Utils.Tuple0));
      });
    };
    var _acceptAnswer = function(from, data) {
      return Task.asyncFunction(function(callback) {
        acceptAnswer(clientId, cm, send, {
          from: from,
          data: data
        });
        callback(Task.succeed(Utils.Tuple0));
      });
    };
    var _addCandidate = function(from, data, upstream) {
      return Task.asyncFunction(function(callback) {
        addCandidate(clientId, cm, send, {
          from: from,
          data: data
        }, upstream);
        callback(Task.succeed(Utils.Tuple0));
      });
    };
    var _closeRemoteStream = function(from, data) {
      return Task.asyncFunction(function(callback) {
        console.log(data);
        closeRemoteStream(cm, from, data.mediaType, function onRemoteVideoURL(from, mediaType, url) {
          localRuntime.setTimeout(function() {
            localRuntime.notify(_onRemoteVideoURL.id, Utils.Tuple2(Tuple3(from, mediaType, 1), url));
          }, 0);
        });
        callback(Task.succeed(Utils.Tuple0));
      });
    };
    var _startStreaming = function(mediaType, peers) {
      peers = listToArray(peers);
      return Task.asyncFunction(function(callback) {
        offerSDP(clientId, cm, send, mediaType, peers, function onLocalVideoURL(mediaType, url) {
          // console.log([mediaType, url]);
          localRuntime.notify(_onLocalVideoURL.id, Utils.Tuple2(mediaType, url));
        });
        callback(Task.succeed(Utils.Tuple0));
      });
    };
    var _endStreaming = function(mediaType) {
      return Task.asyncFunction(function(callback) {
        localRuntime.setTimeout(function() {
          localRuntime.notify(_onLocalVideoURL.id, Utils.Tuple2(mediaType, ""));
        }, 0);
        endStreaming(clientId, cm, send, mediaType);
        callback(Task.succeed(Utils.Tuple0));
      });
    };
    var beforeJoin = function(peerId) {
      return Task.asyncFunction(function(callback) {
        join(clientId, cm, send, peerId);
        callback(Task.succeed(Utils.Tuple0));
      });
    };
    var beforeLeave = function(peerId) {
      return Task.asyncFunction(function(callback) {
        leave(clientId, cm, send, peerId, function onRemoteVideoURL(from, mediaType, url,upstream) {
          localRuntime.setTimeout(function() {
            localRuntime.notify(_onRemoteVideoURL.id, Utils.Tuple2(Tuple3(from, mediaType, upstream?1:0), url));
          }, 0);
        });
        callback(Task.succeed(Utils.Tuple0));
      });
    };

    var currentVolumes = function() {
      var array = cm ? cm.getCurrentVolumes() : [];
      var list = arrayToList(array, function(peerId_volume) {
        return Utils.Tuple2(peerId_volume[0], peerId_volume[1]);
      });
      return list;
    };

    return localRuntime.Native.WebRTC.values = {
      initialize: F2(_initialize),
      answerSDP: F2(_answerSDP),
      acceptAnswer: F2(_acceptAnswer),
      addCandidate: F3(_addCandidate),
      closeRemoteStream: F2(_closeRemoteStream),
      startStreaming: F2(_startStreaming),
      endStreaming: _endStreaming,
      beforeJoin: beforeJoin,
      beforeLeave: beforeLeave,
      requests: requests,
      onLocalVideoURL: _onLocalVideoURL,
      onRemoteVideoURL: _onRemoteVideoURL,
      onAddConnection: _onAddConnection,
      onRemoveConnection: _onRemoveConnection,
      currentVolumes: currentVolumes
    };
  };
  function getRoom() {
    var roomId = decodeURI(location.href.split('/room/')[1].split('?')[0]);
    return roomId;
  }

  function createConnectionManager(connectionOption, send) {
    var connections = {};
    var getConnection = function(id, upstream) {
      if(!id) {
        throw new Error('id is null');
      }
      var key = id + upstream;
      if (!connections[key]) {
        console.log('not exists: ' + key);
        var pc = new RTCPeerConnection(connectionOption);
        connections[key] = pc;
        pc.onicecandidate = function(e) {
          if (e.candidate) {
            // setTimeout(function() {
              send({
                type: upstream ? 'offerCandidate' : 'answerCandidate',
                to: id,
                data: e.candidate
              });
            // }, 0);
          }
        };
      }
      return connections[key];
    };
    var getAllConnections = function() {
      return Object.keys(connections).map(function(key) {
        return connections[key];
      });
    };
    var removeConnection = function(peerId, upstream) {
      delete connections[peerId + upstream];
    };
    var removeAllConnections = function() {
      connections = {};
    };

    //----
    var streams = {};
    var audioAnalysers = {};
    var addStream = function(peerId, mediaType, stream) {

      if(window.AudioContext && mediaType === 'mic') {
        setTimeout(function() {
          var context = new AudioContext();
          var source = context.createMediaStreamSource(stream);
          var gainNode = context.createGain();
          gainNode.gain.value = 100.0;

          var analyser = context.createAnalyser();
          // analyser.fftSize = 1024;
          source.connect(gainNode);
          gainNode.connect(analyser);
          // alert(analyser.minDecibels);
          // alert(analyser.maxDecibels);
          // analyser.minDecibel = -40;
          // analyser.maxDecibels = -10;
          // analyser.minDecibels = parseFloat(document.getElementById("min").value);
          // analyser.maxDecibels = parseFloat(document.getElementById("max").value);
          // analyser.smoothingTimeConstant = parseFloat(document.getElementById("smoothing").value);
          audioAnalysers[peerId] = analyser;
        }, 100)

      }
      streams[peerId + mediaType] = stream;
    };
    var getCurrentVolumes = function() {
      var all = Object.keys(audioAnalysers).map(function(peerId) {
        var analyser = audioAnalysers[peerId];
        var data = new Uint8Array(analyser.frequencyBinCount);
        analyser.getByteTimeDomainData(data);
        var sum = 0;
        for(var i = 0; i < data.length; i++) {
          var value = data[i] - 128;
          sum += value > 0 ? value : -value;
        }
        var value = Math.round(sum / data.length);
        return [peerId, value];
      });
      return all;
    };
    var getStream = function(peerId, mediaType) {
      return streams[peerId + mediaType];
    };
    var removeStream = function(peerId, mediaType) {
      delete streams[peerId + mediaType];
    };
    return {
      getConnection: getConnection,
      getAllConnections: getAllConnections,
      removeConnection: removeConnection,
      removeAllConnections: removeAllConnections,
      getStream: getStream,
      addStream: addStream,
      removeStream: removeStream,
      getCurrentVolumes: getCurrentVolumes
    };
  }

  function offerSDP(clientId, cm, send, mediaType, peers, onLocalVideoURL) {
    var mediaOptions = {};
    if(mediaType === 'mic') {
      mediaOptions.audio = true;
      f(mediaOptions);
    } else if(mediaType === 'video') {
      mediaOptions.video = true;
      f(mediaOptions);
    } else if(mediaType === 'screen') {
      getScreenId(function (error, sourceId, mediaOptions) {
        if(error === 'not-installed') {
          alert('Screen Capturing is not installed.');
          return;
        } else if(error === 'installed-disabled') {
          alert('Screen Capturing is installed but disabled.');
          return;
        } else {
          f(mediaOptions);
        }
      });
    }

    function f(mediaOptions) {
      navigator.getUserMedia(mediaOptions, function(stream) {
        console.log(stream);
        console.log(stream.getAudioTracks());
        console.log(stream.getVideoTracks());

        cm.addStream(clientId, mediaType, stream);
        peers.forEach(function(peerId) {
          sendOfferToPeer(clientId, cm, send, peerId, stream, mediaType);
        });
        onLocalVideoURL(mediaType, URL.createObjectURL(stream));

      }, onerror);
    }

  }

  function sendOfferToPeer(clientId, cm, send, peerId, stream, mediaType) {
    if(peerId === clientId) {
      return;
    }
    var pc = cm.getConnection(peerId, true);
    // pc.onicecandidate = function(e) {
    //   if (e.candidate) {
    //     // setTimeout(function() {
    //       send({
    //         type: 'offerCandidate',
    //         to: peerId,
    //         data: e.candidate
    //       });
    //     // }, 0);
    //   }
    // };

    pc.addStream(stream);



    pc.createOffer(function(offer) {
      // console.log('created offer', offer);

      pc.setLocalDescription(
        new RTCSessionDescription(offer),
        function() { // send offer to server
            send({
              type: 'offerSDP',
              to: peerId,
              data: {
                offer: offer,
                mediaType: mediaType
              }
            });
        }, onerror);
    }, onerror);




  }


  function answerSDP(clientId, cm, send, e, onRemoteVideoURL, onAddConnection, onRemoveConnection) {
    var _from = e.from;
    var pc = cm.getConnection(_from, false);
    var mediaType = e.data.mediaType;
    // pc.onicecandidate = function(e) {
    //   if (e.candidate) {
    //     send({
    //       type: 'answerCandidate',
    //       to: _from,
    //       data: e.candidate
    //     });
    //   }
    // };
    pc.onaddstream = function(e) {
      // var mediaType = e.stream.getAudioTracks()[0] ? 'mic' : 'video';//TODO
      cm.addStream(_from, mediaType, e.stream);
      onRemoteVideoURL(_from, mediaType, URL.createObjectURL(e.stream), false);
    };
    pc.onremovestream = function(e) {
      console.log('onremovestream');
      // console.log(e);
      // cm.removeStream(_from, mediaType);
      // onRemoveConnection(_from, mediaType, 0);
    };
    onAddConnection(_from, mediaType, false);
    pc.setRemoteDescription(
      new RTCSessionDescription(e.data.offer),
      function() {

        pc.createAnswer(function(answer) {
          pc.setLocalDescription(
            new RTCSessionDescription(answer),
            function() {
                send({
                  type: 'answerSDP',
                  to: e.from,
                  data: answer
                });
            }, onerror);
        }, onerror);

      }, onerror);
  }

  function acceptAnswer(clientId, cm, send, e) {
    var pc = cm.getConnection(e.from, true);
    pc.setRemoteDescription(
      new RTCSessionDescription(e.data),
      function() {
      },
      onerror);
  };

  function addCandidate(clientId, cm, send, e, upstream) {
    var pc = cm.getConnection(e.from, upstream);
    console.log('addCandidate:', JSON.stringify(e.data));
    if (e.data.candidate) {
      pc.addIceCandidate(new RTCIceCandidate(e.data));
    }
  }

  function endStreaming(clientId, cm, send, mediaType) {
    var stream = cm.getStream(clientId, mediaType);
    console.log(cm.getAllConnections(), mediaType);
    cm.getAllConnections().forEach(function(pc) {
      try {
        console.log(stream);
        pc.removeStream(stream);
      } catch(e) {
        console.error(e);
      }
    });
    cm.removeStream(clientId, mediaType);
    send({
      type: 'endStream',
      to: clientId,
      data: {
        mediaType: mediaType
      }
    });
  }

  // connect curent streams to joined peer
  function join(clientId, cm, send, from) {
    ["mic", "video", "screen"].forEach(function(mediaType) {
      var stream = cm.getStream(clientId, mediaType);
      if(stream) {
        sendOfferToPeer(clientId, cm, send, from, stream, mediaType);
      }
    });
  }
  // unconnect curent streams to joined peer
  function leave(clientId, cm, send, from, onRemoteVideoURL) {
    cm.removeConnection(from, false);
    ["mic", "video", "screen"].forEach(function(mediaType) {
      closeRemoteStream(cm, from, mediaType, onRemoteVideoURL);
    });
  }

  function closeRemoteStream(cm, remoteClientId, mediaType, onRemoteVideoURL) {
    var pc = cm.getConnection(remoteClientId, true);
    var stream = cm.getStream(remoteClientId, mediaType);
    if(stream) {
      pc.removeStream(stream);
      cm.removeStream(remoteClientId, mediaType);
    }
    onRemoteVideoURL(remoteClientId, mediaType, "", true);
  }

  function polyfill() {
    if (!window.RTCPeerConnection) {
      if (window.mozRTCPeerConnection) {
        window.RTCPeerConnection = window.mozRTCPeerConnection;
      } else if (window.webkitRTCPeerConnection) {
        window.RTCPeerConnection = window.webkitRTCPeerConnection;
      } else {
        alert('this browser does not support WebRTC.');
      }
    }

    if (!window.RTCSessionDescription) {
      if (window.mozRTCSessionDescription) {
        window.RTCSessionDescription = window.mozRTCSessionDescription;
      }
    }

    if (!window.RTCIceCandidate) {
      if (window.mozRTCIceCandidate) {
        window.RTCIceCandidate = window.mozRTCIceCandidate;
      }
    }

    //wrap functions to navigator for Chrome and Firefox
    if (!navigator.getUserMedia) {
      if (navigator.mozGetUserMedia) {
        navigator.getUserMedia = navigator.mozGetUserMedia;
      } else if (navigator.webkitGetUserMedia) {
        navigator.getUserMedia = navigator.webkitGetUserMedia;
      }
    }
    if (!window.AudioContext) {
      if (window.webkitAudioContext) {
        window.AudioContext = window.webkitAudioContext;
      }
    }
  }
  function onerror(e) {
    console.error(e);
  }
  function arrayToList(array, f) {
    var list = {ctor: "[]"};
    for(var i = array.length - 1; i >= 0; i--) {
      list = {
        ctor: "::",
        _0: f(array[i]),
        _1: list
      };
    }
    return list;
  }
  function listToArray(list) {
		var array = [];
		while (list.ctor !== '[]') {
			var entry = list._0;
      array.push(entry);
			list = list._1;
		}
		return array;
	}

  polyfill();

})();
