import React, { useState } from "react";
import { useNavigate } from "react-router-dom";
import api from "../axiosConfig";

function CreateRoom() {
  const [roomName, setRoomName] = useState("");
  const [image, setImage] = useState(null);
  const [message, setMessage] = useState("");
  const navigate = useNavigate();

  const handleCreate = async () => {
    if (roomName.trim() !== "") {
      try {
        const formData = new FormData();
        formData.append("name", roomName);
        if (image) {
          formData.append("image", image);
        }

        await api.post("/chatrooms", formData, {
          headers: {
            "Content-Type": "multipart/form-data",
          },
        });

        setMessage("Room created successfully!");
        setTimeout(() => navigate("/"), 1500);
      } catch (error) {
        console.error("Error creating room:", error);
        setMessage("Failed to create room.");
      }
    } else {
      setMessage("Room name cannot be empty.");
    }
  };

  return (
    <div className="flex items-center justify-center min-h-screen bg-[#1a1b1e] text-white">
      <div className="w-full max-w-md p-8 rounded-xl bg-[#25262b]">
        <div className="flex justify-between items-center mb-8">
          <h1 className="text-2xl font-semibold">Create a Chat Room</h1>
          <button
            onClick={() => navigate("/")}
            className="bg-[#3b82f6] hover:bg-[#2563eb] px-3 py-1.5 rounded-md text-sm font-medium transition-colors"
          >
            Back to Chat
          </button>
        </div>
        
        <div className="space-y-6">
          <div>
            <label className="block text-sm text-gray-300 mb-2">Room Name</label>
            <input
              type="text"
              placeholder="Enter room name"
              className="w-full px-4 py-2.5 rounded-md bg-[#2e2f33] text-white placeholder-gray-400 border border-gray-600/30 focus:outline-none focus:border-[#3b82f6] transition-colors"
              value={roomName}
              onChange={(e) => setRoomName(e.target.value)}
            />
          </div>

          <div>
            <label className="block text-sm text-gray-300 mb-2">Room Image (Optional)</label>
            <div className="w-full px-4 py-2.5 rounded-md bg-[#2e2f33] border border-gray-600/30">
              <input
                type="file"
                accept="image/*"
                className="text-sm text-gray-300 file:mr-4 file:py-1.5 file:px-3 file:rounded-md file:border-0 file:text-sm file:bg-[#3b82f6] file:text-white hover:file:bg-[#2563eb] file:transition-colors"
                onChange={(e) => setImage(e.target.files[0])}
              />
            </div>
          </div>

          <button
            className="w-full bg-[#3b82f6] hover:bg-[#2563eb] transition-colors px-4 py-2.5 rounded-md font-medium text-white"
            onClick={handleCreate}
          >
            Create Room
          </button>

          {message && (
            <div className={`p-3 rounded-md text-center text-sm ${
              message.includes("success") ? "bg-green-900/30 text-green-400" : "bg-red-900/30 text-red-400"
            }`}>
              {message}
            </div>
          )}
        </div>

        <div className="mt-6 text-center">
          <p className="text-gray-400 text-sm">
            Want to join a room instead?{" "}
            <a href="/join-room" className="text-[#3b82f6] hover:text-[#60a5fa] transition-colors">
              Join a chatroom
            </a>
          </p>
        </div>
      </div>
    </div>
  );
}

export default CreateRoom;
