var rooms = {};

var createRoom = function(id) {
  rooms[rooms] = {
    id: id,
    members: []
  };
  return Promise.resolve(rooms[rooms]);
};
var getRoom = function(id) {
  return Promise.resolve(rooms[id]);
};
var getRooms = function() {
  return Promise.resolve(Object.keys(rooms).map(function(key) {
    return rooms[key];
  }));
};
var users = {};
var addUser = function(id, user) {
  users[id] = user;
  return Promise.resolve(user);
};
var getUser = function(id) {
  var user = users[id];
  return Promise.resolve(user);
};

module.exports = {
  createRoom: createRoom,
  getRoom: getRoom,
  getRooms: getRooms,
  addUser: addUser,
  getUser: getUser
};
