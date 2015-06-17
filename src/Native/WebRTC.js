function uuid() {
  var uuid = "", i, random;
  for (i = 0; i < 32; i++) {
    random = Math.random() * 16 | 0;
    if (i == 8 || i == 12 || i == 16 || i == 20) {
      uuid += "-"
    }
    uuid += (i == 12 ? 4 : (i == 16 ? (random & 3 | 8) : random)).toString(16);
  }
  return uuid;
}
var clientId = window.clientId || uuid();//TODO

(function(){

  Elm.Native.WebRTC = {};

  Elm.Native.WebRTC.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.WebRTC = localRuntime.Native.WebRTC || {};
    if (localRuntime.Native.WebRTC.values) return localRuntime.Native.WebRTC.values;

    var Task = Elm.Native.Task.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);
    var NS = Elm.Native.Signal.make(localRuntime);
    var initialRequest = {
      type: "",
      from: "",
      data: {}
    };
    var requests = NS.input('WebRTC.requests', JSON.stringify(initialRequest));
    var _onLocalVideoURL = NS.input('WebRTC.onLocalVideoURL', Utils.Tuple2("", ""));
    var _onRemoteVideoURL = NS.input('WebRTC.onRemoteVideoURL', Utils.Tuple2(Utils.Tuple2("", ""), ""));
    var _onAddConnection = NS.input('WebRTC.onAddConnection',  Utils.Tuple2("", ""));
    var _onRemoveConnection = NS.input('WebRTC.onRemoveConnection',  Utils.Tuple2("", ""));

    var cm = ceateConnectionManager({
      iceServers: [{
        url: 'stun:localhost:3478'
      }]
    });
    var room = getRoom();


    var send = function(data) {
      data.room = room;
      data.from = clientId;
      localRuntime.notify(requests.id, JSON.stringify(data));
    };

    var _answerSDP = function(from, dataString) {
      var data = JSON.parse(dataString);
      return Task.asyncFunction(function(callback) {
        answerSDP(clientId, cm, send, {
          from: from,
          data: data
        }, function onRemoteVideoURL(from, mediaType, url) {
          localRuntime.notify(_onRemoteVideoURL.id, Utils.Tuple2(Utils.Tuple2(from, mediaType), url));
        }, function onAddConnection(from, mediaType) {
          localRuntime.notify(_onAddConnection.id, Utils.Tuple2(from, mediaType));
        }, function onRemoveConnection(from, mediaType) {
          localRuntime.notify(_onRemoveConnection.id, Utils.Tuple2(from, mediaType));
        });
        callback(Task.succeed());
      });
    };
    var _acceptAnswer = function(from, dataString) {
      var data = JSON.parse(dataString);
      return Task.asyncFunction(function(callback) {
        acceptAnswer(clientId, cm, send, {
          from: from,
          data: data
        });
        callback(Task.succeed());
      });
    };
    var _addCandidate = function(from, dataString) {
      var data = JSON.parse(dataString);
      return Task.asyncFunction(function(callback) {
        addCandidate(clientId, cm, send, {
          from: from,
          data: data
        });
        callback(Task.succeed());
      });
    };
    var _closeRemoteStream = function(from, dataString) {
      var data = JSON.parse(dataString);
      return Task.asyncFunction(function(callback) {
        closeRemoteStream(cm, from, mediaType, function onRemoteVideoURL(from, mediaType, url) {
          localRuntime.notify(_onRemoteVideoURL.id, Utils.Tuple2(Utils.Tuple2(from, mediaType), url));
        });
        callback(Task.succeed());
      });
    };
    var startStreaming = function(mediaType, peers) {
      peers = listToArray(peers);
      return Task.asyncFunction(function(callback) {
        offerSDP(clientId, cm, send, mediaType, peers, function onLocalVideoURL(mediaType, url) {
          console.log([mediaType, url]);
          localRuntime.notify(_onLocalVideoURL.id, Utils.Tuple2(mediaType, url));
        });

        callback(Task.succeed());
      });
    };
    var endStreaming = function(mediaType) {
      return Task.asyncFunction(function(callback) {
        localRuntime.notify(_onLocalVideoURL.id, Utils.Tuple2(mediaType, null));
        endStreaming(clientId, cm, send, mediaType);
        callback(Task.succeed());
      });
    };
    var beforeJoin = function(peerId) {
      return Task.asyncFunction(function(callback) {
        join(clientId, cm, send, peerId);
        callback(Task.succeed());
      });
    };
    var beforeLeave = function(peerId) {
      return Task.asyncFunction(function(callback) {
        leave(clientId, cm, send, peerId, function onRemoteVideoURL(from, mediaType, url) {
          localRuntime.notify(_onRemoteVideoURL.id, Utils.Tuple2(Utils.Tuple2(from, mediaType), url));
        });
        callback(Task.succeed());
      });
    };

    return localRuntime.Native.WebRTC.values = {
      answerSDP: F2(_answerSDP),
      acceptAnswer: F2(_acceptAnswer),
      addCandidate: F2(_addCandidate),
      closeRemoteStream: F2(_closeRemoteStream),
      startStreaming: F2(startStreaming),
      endStreaming: endStreaming,
      beforeJoin: beforeJoin,
      beforeLeave: beforeLeave,
      requests: requests,
      onLocalVideoURL: _onLocalVideoURL,
      onRemoteVideoURL: _onRemoteVideoURL,
      onAddConnection: _onAddConnection,
      onRemoveConnection: _onRemoveConnection,
    };
  };
  function getRoom() {
    var roomId = decodeURI(location.href.split('/room/')[1].split('?')[0]);
    return roomId;
  }

  function ceateConnectionManager(connectionOption) {
    var connections = {};//TODO upstream/downstream
    var getConnection = function(id) {
      if(!id) {
        throw new Error('id is null');
      }
      if (!connections[id]) {
        console.log('not exists: ' + id);
        var pc = new RTCPeerConnection(connectionOption);
        connections[id] = pc;
      }
      return connections[id];
    };
    var getAllConnections = function() {
      return Object.keys(connections).map(function(key) {
        return connections[key];
      });
    };
    var removeConnection = function(peerId) {
      delete connections[peerId];
    };
    var removeAllConnections = function(){
      connections = {};
    };

    //----
    var streams = {};
    var addStream = function(peerId, mediaType, stream) {
      streams[peerId + mediaType] = stream;
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
      removeStream: removeStream
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
        cm.addStream(clientId, mediaType, stream);
        peers.forEach(function(peerId) {
          sendOfferToPeer(clientId, cm, send, peerId, stream, function() {

          });
        });
        onLocalVideoURL(mediaType, URL.createObjectURL(stream));

      }, onerror);
    }



  }

  function sendOfferToPeer(clientId, cm, send, peerId, stream, cb) {
    if(peerId === clientId) {
      return;
    }

    var pc = cm.getConnection(peerId);
    pc.onicecandidate = function(e) {
      if (e.candidate) {
        send({
          type: 'offerCandidate',
          to: peerId,
          data: e.candidate
        });
      }
    };

    pc.addStream(stream);

    pc.createOffer(function(offer) {
      console.log('created offer', offer);

      pc.setLocalDescription(
        new RTCSessionDescription(offer),
        function() { // send offer to server
          send({
            type: 'offerSDP',
            to: peerId,
            data: offer
          });
        }, onerror);
      cb && cb();
    }, onerror);
  }


  function answerSDP(clientId, cm, send, e, onRemoteVideoURL, onAddConnection, onRemoveConnection) {
    var _from = e.from;
    var pc = cm.getConnection(_from);
    var mediaType = "video";//TODO
    pc.onicecandidate = function(e) {
      if (e.candidate) {
        send({
          type: 'answerCandidate',
          to: _from,
          data: e.candidate
        });
      }
    };
    pc.onaddstream = function(e) {
      cm.addStream(_from, mediaType, e.stream);
      onRemoteVideoURL(_from, mediaType, URL.createObjectURL(e.stream));
    };
    pc.onremovestream = function(e) {
      console.log('onremovestream');
      pc.close();
      cm.removeConnection(_from);
      cm.removeStream(_from, mediaType);

      onRemoveConnection(_from, mediaType);
      // roomSignal.ports.removeConnection.send([_from, mediaType]);
    };
    onAddConnection(_from, mediaType);
    // roomSignal.ports.addConnection.send([_from, mediaType]);
    pc.setRemoteDescription(
      new RTCSessionDescription(e.data),
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
    var pc = cm.getConnection(e.from);
    pc.setRemoteDescription(
      new RTCSessionDescription(e.data),
      function() {
      },
      onerror);
  };

  function addCandidate(clientId, cm, send, e) {
    var pc = cm.getConnection(e.from);
    if (e.data.candidate) {
      pc.addIceCandidate(new RTCIceCandidate(e.data));
    }
  }

  function endStreaming(clientId, cm, send, mediaType) {
    var stream = cm.getStream(clientId, mediaType);
    cm.getAllConnections().forEach(function(pc) {
      try {
        pc.removeStream(stream);
      } catch(e) {
        console.error(e);
      }
      pc.close();
    });
    cm.removeStream(clientId, mediaType);
    cm.removeAllConnections();
    send({
      type: 'endStream',
      to: clientId,
      mediaType: mediaType,
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
        sendOfferToPeer(clientId, cm, send, from, stream);
      }
    });
  }
  // unconnect curent streams to joined peer
  function leave(clientId, cm, send, from, onRemoteVideoURL) {
    cm.removeConnection(from);
    ["mic", "video", "screen"].forEach(function(mediaType) {
      closeRemoteStream(cm, from, mediaType, onRemoteVideoURL);
    });
  }

  function closeRemoteStream(cm, remoteClientId, mediaType, onRemoteVideoURL) {
    var pc = cm.getConnection(remoteClientId);
    var stream = cm.getStream(remoteClientId, mediaType);
    if(stream) {
      pc.removeStream(stream);
      cm.removeStream(remoteClientId, mediaType);
    }
    pc.close();
    cm.removeConnection(remoteClientId);
    onRemoteVideoURL(remoteClientId, mediaType, null);
  }

  function polyfill() {
    if (!window.RTCPeerConnection) {
      if (window.mozRTCPeerConnection) {
        window.RTCPeerConnection = window.mozRTCPeerConnection;
      } else if (webkitRTCPeerConnection) {
        window.RTCPeerConnection = window.webkitRTCPeerConnection;
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

  function listToArray(list) {
		var array = [];
		while (list.ctor !== '[]') {
			var entry = list._0;
      array.push(entry);
			list = list._1;
		}
    console.log(array);
		return array;
	}

  polyfill();

})();
