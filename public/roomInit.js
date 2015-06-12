function getRoom() {
  var roomId = decodeURI(location.href.split('/room/')[1].split('?')[0]);
  return roomId;
}
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
function setupWebSocket(room, clientId, onmessage) {
  var send = function(data) {
    data.room = getRoom();
    data.from = clientId;
    // conosle.log(data);
    roomSignal.ports.wssend.send(JSON.stringify(data));
  };
  roomSignal.ports.wsmessage.subscribe(function(data) {
    console.log(data);
    onmessage(JSON.parse(data));
  });
  roomSignal.ports.wsopened.subscribe(function(opened) {
    if(opened) {
      var time = new Date().getTime();
      send({
        type: 'join',
        time: time
      });
    }
  });
  return send;
}



function polyfill() {
  //---------------
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
//--------------

function onerror(e) {
  console.error(e);
}

function ceateConnectionManager() {
  var connections = {};//TODO upstream/downstream
  var getConnection = function(id) {
    if(!id) {
      throw new Error('id is null');
    }
    if (!connections[id]) {
      console.log('not exists: ' + id);
      var pc = new RTCPeerConnection({
        iceServers: [{
          url: 'stun:localhost:3478'
        }]
      });
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


function offerSDP(clientId, cm, send, mediaType, peers) {
  var mediaOptions = {};
  if(mediaType === 'mic') {
    mediaOptions.audio = true;
    f(mediaOptions);
  } else if(mediaType === 'video') {
    mediaOptions.video = true;
    f(mediaOptions);
  } else if(mediaType === 'screen') {
    getScreenId(function (error, sourceId, mediaOptions) {
      if(!error) {
        f(mediaOptions);
      }
    });
  }

  function f(mediaOptions) {
    navigator.getUserMedia(mediaOptions, function(stream) {
      cm.addStream(clientId, mediaType, stream);
      roomSignal.ports.setLocalVideoUrl.send([mediaType, URL.createObjectURL(stream)]);

      peers.forEach(function(peerId) {
        sendOfferToPeer(clientId, cm, send, peerId, stream, function() {

        });
      });
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


function answerSDP(clientId, cm, send, e) {
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
    roomSignal.ports.setVideoUrl.send([[_from, mediaType], URL.createObjectURL(e.stream)]);
  };
  pc.onremovestream = function(e) {
    console.log('onremovestream');
    pc.close();
    cm.removeConnection(_from);
    cm.removeStream(_from, mediaType);
    roomSignal.ports.removeConnection.send([_from, mediaType]);
  };
  roomSignal.ports.addConnection.send([_from, mediaType]);
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
  roomSignal.ports.setLocalVideoUrl.send([mediaType, null]);
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
    mediaType: mediaType
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
function leave(clientId, cm, send, from) {
  cm.removeConnection(from);
  ["mic", "video", "screen"].forEach(function(mediaType) {
    closeRemoteStream(cm, from, mediaType);
  });
}

function closeRemoteStream(cm, remoteClientId, mediaType) {
  var pc = cm.getConnection(remoteClientId);
  var stream = cm.getStream(remoteClientId, mediaType);
  if(stream) {
    pc.removeStream(stream);
    cm.removeStream(remoteClientId, mediaType);
  }
  pc.close();
  cm.removeConnection(remoteClientId);
  roomSignal.ports.setVideoUrl.send([[remoteClientId, mediaType], null]);
}

function onMessage(clientId, cm, send, data) {
  if (data.type === 'offerSDP') {
    answerSDP(clientId, cm, send, data);
  } else if (data.type === 'answerSDP') {
    acceptAnswer(clientId, cm, send, data);
  } else if (data.type === 'offerCandidate') {
    addCandidate(clientId, cm, send, data);
  } else if (data.type === 'answerCandidate') {
    addCandidate(clientId, cm, send, data);
  } else if (data.type === 'endStream') {
    closeRemoteStream(cm, data.from, data.mediaType);
  }
};

//----------
polyfill();
var roomSignal = Elm.fullscreen(Elm.Main, {
  updateRoom: getRoom(),
  websocketRunner: [],
  addConnection: ["",""],
  removeConnection: ["",""],
  setVideoUrl: [["", ""],""],
  setLocalVideoUrl: ["", ""],
  wssend: ""
});
roomSignal.ports.initRoom.subscribe(function(initial) {
  console.log('initRoom')

  var room = initial.room;
  var cm = ceateConnectionManager();
  var clientId = uuid();

  var send = setupWebSocket(room, clientId, function(e) {
    onMessage(clientId, cm, send, e);
  });
  roomSignal.ports.startStreaming.subscribe(function(args) {
    var mediaType = args[0];
    var peers = args[1];
    offerSDP(clientId, cm, send, mediaType, peers);
  });
  roomSignal.ports.endStreaming.subscribe(function(args) {
    var mediaType = args[0];
    var peers = args[1];
    endStreaming(clientId, cm, send, mediaType);
  });
  roomSignal.ports.beforeJoin.subscribe(function(peerId) {
    join(clientId, cm, send, peerId);
  });
  roomSignal.ports.beforeLeave.subscribe(function(peerId) {
    leave(clientId, cm, send, peerId);
  });
  roomSignal.ports.sendChat.subscribe(function(message) {
    var time = new Date().getTime();
    send({
      type: 'message',
      time: time,
      message: message
    });
  });
  send({
    type: 'join',
    from: clientId
  })
});
