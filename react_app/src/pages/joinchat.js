import React, { useState, useEffect } from "react";
import { useNavigate } from "react-router-dom";
import api from "../axiosConfig";

function JoinRoom() {
  const [chatRooms, setChatRooms] = useState([]);
  const [message, setMessage] = useState("");
  const [loading, setLoading] = useState(true);
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(1);
  const roomsPerPage = 5;
  const navigate = useNavigate();

  useEffect(() => {
    const userId = localStorage.getItem("userId");
    if (!userId) {
      navigate("/login");
      return;
    }
    loadRooms();
  }, [navigate]);

  const loadRooms = async () => {
    try {
      const response = await api.get(`/userchatroom/unjoined/${localStorage.getItem("userId")}`);
      const data = response.data;
      
      if (Array.isArray(data) && data.length > 0) {
        setChatRooms(data);
        setTotalPages(Math.ceil(data.length / roomsPerPage));
      } else {
        setMessage("No available chat rooms to join.");
      }
    } catch (error) {
      console.error("Error fetching unjoined chat rooms:", error);
      setMessage("Failed to fetch chat rooms.");
    } finally {
      setLoading(false);
    }
  };

  const handleJoin = async (roomId) => {
    const userId = localStorage.getItem("userId");
    if (!userId) {
      navigate("/login");
      return;
    }

    try {
      await api.post("/userchatroom", {
        userId: parseInt(userId),
        chatroomId: roomId,
        role: "MEMBER"
      });
      
      setMessage("Joined successfully!");
      setTimeout(() => navigate("/"), 1500);
    } catch (error) {
      console.error("Error joining room:", error);
      setMessage("Failed to join room. You might already be a member.");
    }
  };

  const currentRooms = chatRooms.slice(
    (currentPage - 1) * roomsPerPage,
    currentPage * roomsPerPage
  );

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen bg-gray-900 text-white">
        <div className="animate-spin rounded-full h-12 w-12 border-t-2 border-b-2 border-blue-500"></div>
      </div>
    );
  }

  return (
    <div className="flex items-center justify-center min-h-screen bg-gray-900 text-white px-4">
      <div className="bg-gray-800 p-8 rounded-2xl shadow-lg w-full max-w-2xl">
        <div className="flex justify-between items-center mb-6">
          <h1 className="text-3xl font-bold">Join a Chat Room</h1>
          <button
            onClick={() => navigate("/")}
            className="bg-blue-500 hover:bg-blue-600 px-4 py-2 rounded-lg transition-colors text-sm"
          >
            Back to Chat
          </button>
        </div>

        {message && (
          <div className={`mb-6 p-4 rounded-lg text-center ${
            message.includes("success") ? "bg-green-900/50 text-green-300" : "bg-red-900/50 text-red-300"
          }`}>
            {message}
          </div>
        )}

        <div className="space-y-4 mb-6">
          {currentRooms.length > 0 ? (
            currentRooms.map((room) => (
              <div key={room.id} className="flex items-center justify-between bg-gray-700 p-4 rounded-lg">
                <div className="flex items-center gap-4">
                  <img
                    src={room.imageUrl ? `http://localhost:8080/chatrooms/image/${room.imageUrl}` : "/default-room.png"}
                    alt="room"
                    className="w-12 h-12 rounded-full object-cover"
                  />
                  <div>
                    <h3 className="font-semibold">{room.name}</h3>
                    <p className="text-sm text-gray-400">Created by: {room.createdBy.username || "Unknown"}</p>
                  </div>
                </div>
                <button
                  className="bg-blue-500 hover:bg-blue-600 px-4 py-2 rounded-lg transition-colors"
                  onClick={() => handleJoin(room.id)}
                >
                  Join
                </button>
              </div>
            ))
          ) : (
            <div className="text-center text-gray-400 py-8">
              No available chat rooms to join.
            </div>
          )}
        </div>

        {totalPages > 1 && (
          <div className="flex justify-center gap-2 mt-4">
            {Array.from({ length: totalPages }, (_, i) => i + 1).map((page) => (
              <button
                key={page}
                onClick={() => setCurrentPage(page)}
                className={`px-3 py-1 rounded ${
                  currentPage === page
                    ? "bg-blue-500 text-white"
                    : "bg-gray-700 text-gray-300 hover:bg-gray-600"
                }`}
              >
                {page}
              </button>
            ))}
          </div>
        )}

        <div className="mt-8 text-center">
          <p className="text-gray-400">
            Want to create your own room?{" "}
            <a href="/create-room" className="text-blue-400 hover:underline">
              Create a chatroom
            </a>
          </p>
        </div>
      </div>
    </div>
  );
}

export default JoinRoom;
