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
var addUser = function(id, name, email) {
  var user = {
    id: id,
    name: name,
    email: email
  };
  users[id] = user;
  return Promise.resolve(user);
};
var getUser = function(id) {
  return Promise.resolve(users[id]);
};

module.exports = {
  createRoom: createRoom,
  getRoom: getRoom,
  getRooms: getRooms,
  addUser: addUser,
  getUser: getUser
};
