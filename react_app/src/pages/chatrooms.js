import React from "react";
import { Link } from "react-router-dom";

function ChatRooms() {
  const rooms = ["General", "Tech Talk", "Gaming", "Movies"]; // Example rooms

  return (
    <div className="flex flex-col items-center justify-center h-screen bg-gray-900 text-white">
      <h1 className="text-3xl font-bold mb-4">Chat Rooms</h1>
      <ul className="w-80">
        {rooms.map((room, index) => (
          <li key={index} className="p-3 my-2 bg-gray-700 rounded-md text-center">
            <Link to={`/chat/${room}`} className="text-blue-400">{room}</Link>
          </li>
        ))}
      </ul>
      <div className="mt-4">
        <Link to="/create-room" className="bg-green-500 px-4 py-2 rounded-md mr-2">Create Room</Link>
        <Link to="/join-room" className="bg-blue-500 px-4 py-2 rounded-md">Join Room</Link>
      </div>
    </div>
  );
}

export default ChatRooms;
