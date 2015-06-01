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
  var connection = new WebSocket('ws://localhost:9999/ws', ['soap', 'xmpp']);

  var send = function(data) {
    data.room = getRoom();
    data.from = clientId;
    connection.send(JSON.stringify(data));
  };
  connection.onopen = function() {
    console.log('open');
    var time = new Date().getTime();
    send({
      type: 'join',
      time: time
    });
  };
  connection.onclose = function() {
    console.log('close');
  };
  connection.onmessage = function(e) {
    var data = JSON.parse(e.data);
    onmessage(data);
  };
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
      console.log(connections);
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


function getRoomInfo(cb) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/api/room/' + getRoom(), true);
  xhr.responseType = 'json';
  xhr.onload = function(e) {
    if (this.status == 200) {
      cb(this.response);
    }
  };
  xhr.send();
}


function offerSDP(clientId, cm, send, mediaType) {
  var mediaOptions = {};
  if(mediaType === 'mic') {
    mediaOptions.audio = true;
  } else if(mediaType === 'video') {
    mediaOptions.video = true;
  } else if(mediaType === 'screen') {
  }

  navigator.getUserMedia(mediaOptions, function(stream) {
    cm.addStream(clientId, mediaType, stream);
    roomSignal.ports.setLocalVideoUrl.send([mediaType, URL.createObjectURL(stream)]);

    getRoomInfo(function(room) {
      var peers = room.peers;
      peers.forEach(function(peerId) {
        sendOfferToPeer(clientId, cm, send, peerId, stream, function(){

        });
      });
    });

  }, onerror);

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
  console.log(e.from);
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
    pc.removeStream(stream);
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

function join(clientId, cm, send, e, cb) {
  ["mic", "video", "screen"].forEach(function(mediaType) {
    var stream = cm.getStream(clientId, mediaType);
    if(stream) {
      sendOfferToPeer(clientId, cm, send, e.from, stream);
    }
  });
  cb();
}
function leave(clientId, cm, send, e, cb) {
  cm.removeConnection(e.from);
  ["mic", "video", "screen"].forEach(function(mediaType) {
    closeRemoteStream(cm, e.from, mediaType);
  });
  cb();
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

function requestFullScreen(videoUrl) {
  var video = document.querySelector('video[src="' + videoUrl + '"]');
  if (video.requestFullScreen) {
    video.requestFullScreen();
  } else if (video.mozRequestFullScreen) {
    video.mozRequestFullScreen();
  } else if (video.webkitRequestFullScreen) {
    video.webkitRequestFullScreen();
  }
}


//----------
polyfill();
var roomSignal = Elm.fullscreen(Elm.Main, {
  receiveChat: {
    userId: '',
    message: ''
  },
  updateRoom: getRoom(),
  addConnection: ["",""],
  removeConnection: ["",""],
  setVideoUrl: [["", ""],""],
  setLocalVideoUrl: ["", ""],
  join: null,
  leave: "",
  initRoom: "",
  setRoomName: ""
});
getRoomInfo(function(room) {

  var cm = ceateConnectionManager();
  var clientId = uuid();

  roomSignal.ports.setRoomName.send(room.id);
  room.peers.forEach(function(peerId) {
    var user = room.users[peerId];
    roomSignal.ports.join.send([peerId, user]);
  });

  var send = setupWebSocket(room, clientId, function(e) {
    var type = e.type;
    console.log(type)
    if (type === 'update') {
      roomSignal.ports.updateRoom.send(getRoom());
    } else if (type === 'message') {
      roomSignal.ports.receiveChat.send(e.message);
    } else if (type === 'join') {
      join(clientId, cm, send, e, function() {
        roomSignal.ports.join.send([e.from, e.user]);
      });
    } else if (type === 'leave') {
      leave(clientId, cm, send, e, function() {
        roomSignal.ports.leave.send(e.from);
      });
    } else {
      onMessage(clientId, cm, send, e);
    }
  });
  roomSignal.ports.startStreaming.subscribe(function(mediaType) {
    offerSDP(clientId, cm, send, mediaType);
  });
  roomSignal.ports.endStreaming.subscribe(function(mediaType) {
    endStreaming(clientId, cm, send, mediaType);
  });
  roomSignal.ports.sendChat.subscribe(function(message) {
    var time = new Date().getTime();
    send({
      type: 'message',
      time: time,
      message: message
    });
  });

  roomSignal.ports.requestFullScreen.subscribe(function(videoUrl) {
    requestFullScreen(videoUrl);
  });
});
