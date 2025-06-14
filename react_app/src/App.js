import React, { useState, useEffect, useRef } from "react";
import { BrowserRouter as Router, Route, Routes, useNavigate, Navigate } from "react-router-dom";
import Login from "./pages/login";
import Signup from "./pages/signup";
import CreateRoom from "./pages/createchat";
import JoinRoom from "./pages/joinchat";
import api from "./axiosConfig";
import EditProfile from "./pages/editprofile";

// Add axios interceptor for 403 handling
api.interceptors.response.use(
  (response) => response,
  (error) => {
    if (error.response && error.response.status === 403) {
      localStorage.removeItem("userId");
      localStorage.removeItem("username");
      localStorage.removeItem("image_url");
      window.location.href = "/login";
    }
    return Promise.reject(error);
  }
);

// Protected Route Component
function ProtectedRoute({ children }) {
  const navigate = useNavigate();
  const [isLoading, setIsLoading] = useState(true);
  const [isAuthenticated, setIsAuthenticated] = useState(false);

  useEffect(() => {
    const checkAuth = async () => {
      const userId = localStorage.getItem("userId");
      if (!userId) {
        setIsAuthenticated(false);
        setIsLoading(false);
        return;
      }

      try {
        // Verify the user's session
        await api.get(`/users/${userId}`);
        setIsAuthenticated(true);
      } catch (error) {
        localStorage.removeItem("userId");
        localStorage.removeItem("username");
        localStorage.removeItem("image_url");
        setIsAuthenticated(false);
      } finally {
        setIsLoading(false);
      }
    };

    checkAuth();
  }, [navigate]);

  if (isLoading) {
    return (
      <div className="flex items-center justify-center min-h-screen bg-gray-900">
        <div className="text-white text-lg">Loading...</div>
      </div>
    );
  }

  return isAuthenticated ? children : <Navigate to="/login" />;
}

function Chat({ currentRoom }) {
  const [message, setMessage] = useState("");
  const [messages, setMessages] = useState([]);
  const [username, setUsername] = useState(localStorage.getItem("username") || "User");
  const [loading, setLoading] = useState(false);
  const [hasMore, setHasMore] = useState(true);
  const [page, setPage] = useState(0);
  const chatEndRef = useRef(null);
  const ws = useRef(null);

  // Helper function to scroll to bottom
  const scrollToBottom = (behavior = "smooth") => {
    chatEndRef.current?.scrollIntoView({ behavior });
  };

  // Helper function to sort messages by timestamp
  const sortMessagesByTimestamp = (msgs) => {
    return [...msgs].sort((a, b) => new Date(a.timestamp) - new Date(b.timestamp));
  };

  const loadMessages = async (pageNum = 0) => {
    if (!currentRoom?.chatRoom?.id || loading) return;
    
    setLoading(true);
    try {
      const response = await api.get(`/messages/chatroom/${currentRoom.chatRoom.id}/history?page=${pageNum}&size=20`);
      const newMessages = response.data.map(msg => ({
        name: msg.user.username,
        text: msg.message,
        imageUrl: msg.user.image_url || null,
        timestamp: msg.timestamp,
      }));

      if (pageNum === 0) {
        // Initial load - sort messages by timestamp
        setMessages(sortMessagesByTimestamp(newMessages));
        // Scroll to bottom immediately after initial load
        setTimeout(() => scrollToBottom("auto"), 100);
      } else {
        // Merge new messages with existing ones and sort
        setMessages(prev => sortMessagesByTimestamp([...prev, ...newMessages]));
      }

      setHasMore(newMessages.length === 20);
      setPage(pageNum);
    } catch (error) {
      console.error("Error loading messages:", error);
    } finally {
      setLoading(false);
    }
  };

  // Effect to handle initial scroll and message updates
  useEffect(() => {
    if (messages.length > 0 && page === 0) {
      scrollToBottom("auto");
    }
  }, [messages, page]);

  useEffect(() => {
    if (currentRoom && currentRoom.chatRoom.id) {
      // Reset states when changing rooms
      setMessages([]);
      setPage(0);
      setHasMore(true);
      
      // Load initial messages
      loadMessages(0);

      // Set up WebSocket connection
      const token = localStorage.getItem("token"); // <-- must be stored after login
      ws.current = new WebSocket(`ws://localhost:4000/websocket`);


      ws.current.onopen = () => {
        const joinMessage = JSON.stringify({
          action: "join",
          userId: localStorage.getItem("userId"),
          chatRoomId: currentRoom.chatRoom.id,
          token: localStorage.getItem("token")
        });
        ws.current.send(joinMessage);
      };

      ws.current.onmessage = (event) => {
        try {
          const data = JSON.parse(event.data);
          if (data.message && data.username) {
            const newMessage = {
              name: data.username,
              text: data.message,
              imageUrl: data.imageUrl === "null" ? null : data.imageUrl,
              timestamp: data.timestamp || new Date().toISOString(),
            };
            
            // Add new message and ensure correct order
            setMessages(prev => sortMessagesByTimestamp([...prev, newMessage]));
            // Scroll to bottom for new messages
            setTimeout(() => scrollToBottom(), 100);
          }
        } catch (error) {
          console.error("Error parsing WebSocket message:", error);
        }
      };

      ws.current.onerror = (error) => console.error("WebSocket error:", error);
      ws.current.onclose = () => console.log("Disconnected from Erlang WebSocket");

      return () => { if (ws.current) ws.current.close(); };
    }
  }, [currentRoom, username]);

  const sendMessage = () => {
    if (message.trim() === "") return;

    const msgObj = {
      action: "message",
      userId: localStorage.getItem("userId"),
      chatRoomId: currentRoom.chatRoom.id,
      message,
      username: localStorage.getItem("username"),
      imageUrl: localStorage.getItem("image_url"),
      token: localStorage.getItem("token")
    };

    if (ws.current && ws.current.readyState === WebSocket.OPEN) {
      ws.current.send(JSON.stringify(msgObj));
      setMessage("");
      // Scroll to bottom after sending a message
      setTimeout(() => scrollToBottom(), 100);
    } else {
      console.error("WebSocket not connected");
    }
  };

  const handleKeyDown = (e) => {
    if (e.key === "Enter") sendMessage();
  };

  return (
    <div className="flex-1 flex flex-col justify-between h-full bg-[radial-gradient(ellipse_at_top_right,_var(--tw-gradient-stops))] from-gray-700/90 via-gray-800/90 to-gray-900/90">
      <div className="flex-1 overflow-y-auto px-4 py-6 space-y-3" style={{ maxHeight: 'calc(100vh - 140px)' }}>
        {hasMore && !loading && messages.length > 0 && (
          <div className="sticky top-0 z-10 flex justify-center py-2">
            <button
              onClick={() => loadMessages(page + 1)}
              className="bg-blue-500/80 hover:bg-blue-600/80 text-white px-4 py-2 rounded-full text-sm font-medium shadow-lg transition-all duration-200 backdrop-blur-sm"
            >
              Load More Messages
            </button>
          </div>
        )}

        {loading && page > 0 && (
          <div className="text-center text-gray-300 py-2">
            Loading older messages...
          </div>
        )}

        {messages.length > 0 ? messages.map((msg, index) => {
          const isYou = msg.name === username;
          const avatarUrl = msg.imageUrl && msg.imageUrl !== "null"
            ? `http://localhost:8080/api/images/${msg.imageUrl}`
            : "/default-avatar.jpg";

          return (
            <div key={index} className={`flex items-end gap-2 ${isYou ? "justify-end" : "justify-start"} w-full`}>
              {!isYou && (
                <img src={avatarUrl} alt="avatar" className="w-10 h-10 rounded-full object-cover border border-gray-500/30 shadow-md" />
              )}
              <div className={`p-3 rounded-2xl max-w-[70%] break-words shadow-lg backdrop-blur-sm ${
                isYou 
                  ? "bg-gradient-to-br from-blue-500/90 to-blue-600/90 text-white rounded-br-none" 
                  : "bg-gradient-to-br from-gray-600/90 to-gray-700/90 text-white rounded-bl-none"
              }`}>
                <div className="text-sm font-semibold mb-1 opacity-90">{isYou ? "You" : msg.name}</div>
                <div className="text-base whitespace-pre-wrap">{msg.text}</div>
                {msg.timestamp && (
                  <div className="text-xs text-gray-200/90 mt-1 text-right">
                    {new Date(msg.timestamp).toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" })}
                  </div>
                )}
              </div>
            </div>
          );
        }) : (
          <div className="text-center text-gray-300 py-8">
            No messages yet. Start the conversation!
          </div>
        )}
        <div ref={chatEndRef}></div>
      </div>

      <div className="bg-gradient-to-r from-gray-700/90 to-gray-800/90 border-t border-gray-600/20 p-4 shadow-lg backdrop-blur-sm">
        <div className="flex gap-2">
          <input
            type="text"
            className="flex-1 px-4 py-3 rounded-lg bg-white/10 text-white placeholder-gray-400 border border-gray-500/30 focus:outline-none focus:ring-2 focus:ring-blue-400/50 shadow-inner backdrop-blur-sm"
            placeholder="Type a message..."
            value={message}
            onChange={(e) => setMessage(e.target.value)}
            onKeyDown={handleKeyDown}
          />
          <button
            onClick={sendMessage}
            className="bg-blue-500/90 hover:bg-blue-600/90 text-white px-6 py-3 rounded-lg font-semibold transition-all duration-200 shadow-md hover:shadow-lg backdrop-blur-sm"
          >
            Send
          </button>
        </div>
      </div>
    </div>
  );
}

function ChatRooms() {
  const [chatRooms, setChatRooms] = useState([]);
  const [currentRoom, setCurrentRoom] = useState(null);
  const [loading, setLoading] = useState(true);
  const [showEditModal, setShowEditModal] = useState(false);
  const [editName, setEditName] = useState("");
  const [editImage, setEditImage] = useState(null);
  const navigate = useNavigate();

  useEffect(() => {
    const userId = localStorage.getItem("userId");
    if (!userId) {
      navigate("/login");
      return;
    }

    api.get(`/userchatroom/${userId}`)
      .then((response) => {
        const data = response.data;
        if (Array.isArray(data) && data.length > 0) {
          setChatRooms(data);
          setCurrentRoom(data[0]);
        } else {
          navigate("/join-room");
        }
      })
      .catch(() => navigate("/join-room"))
      .finally(() => setLoading(false));
  }, [navigate]);

  const handleUpdateChatroom = async () => {
    try {
      const formData = new FormData();
      formData.append("name", editName);
      if (editImage) formData.append("image", editImage);

      const res = await api.put(`/chatrooms/${currentRoom.chatRoom.id}`, formData, {
        headers: { "Content-Type": "multipart/form-data" }
      });

      const updatedRoom = {
        ...currentRoom,
        chatRoom: {
          ...currentRoom.chatRoom,
          name: res.data.name,
          imageUrl: res.data.imageUrl
        }
      };
      setCurrentRoom(updatedRoom);
      setShowEditModal(false);
    } catch (err) {
      console.error("Update failed:", err);
    }
  };

  if (loading) return <div className="text-gray-700 p-4">Loading chat rooms...</div>;
  const imageUrl = localStorage.getItem("image_url");
  const validImage = imageUrl && imageUrl !== "null" && imageUrl !== "undefined";
  return (
    <div className="flex h-screen bg-gradient-to-br from-slate-50 via-blue-50 to-slate-50 text-gray-900">
      {/* Left Sidebar */}
      <div className="w-1/4 bg-white/70 border-r border-slate-200/50 flex flex-col h-screen shadow-lg backdrop-blur-sm">
        <div className="p-4 border-b border-slate-200/50 shrink-0 bg-gradient-to-r from-white/70 to-blue-50/70">
          <h2 className="text-xl font-bold text-gray-800">Chat Rooms</h2>
        </div>

        <div className="flex-1 overflow-y-auto px-4 py-2 bg-white/30">
          <ul className="space-y-2">
            {chatRooms.map((room, index) => (
              <li
                key={index}
                className={`p-3 rounded-lg cursor-pointer transition-all duration-200 backdrop-blur-sm ${
                  currentRoom && currentRoom.chatRoom.id === room.chatRoom.id
                    ? "bg-gradient-to-r from-blue-100/80 to-blue-50/80 text-blue-900 font-semibold shadow-sm"
                    : "hover:bg-white/50 hover:shadow-sm bg-white/30"
                }`}
                onClick={() => setCurrentRoom(room)}
              >
                <div className="flex items-center gap-3">
                  <img
                    src={room.chatRoom.imageUrl ? `http://localhost:8080/chatrooms/image/${room.chatRoom.imageUrl}` : "/default-room.png"}
                    alt="room"
                    className="w-10 h-10 rounded-full object-cover object-center border border-slate-200/50 shadow-sm"
                  />
                  <span>{room.chatRoom.name}</span>
                </div>
              </li>
            ))}
          </ul>
        </div>

        <div className="p-4 border-t border-slate-200/50 flex items-center justify-between shrink-0 bg-gradient-to-r from-white/70 to-blue-50/70 backdrop-blur-sm">
          <button onClick={() => navigate('/profile')} className="hover:opacity-80 transition-opacity">
            <img src="/settings-svgrepo-com.svg" alt="Settings" className="w-6 h-6" />
          </button>
          <img
            src={
              validImage
                ? `http://localhost:8080/api/images/${imageUrl}`
                : "/default-avatar.jpg"
            }
            alt="Profile"
            className="w-10 h-10 rounded-full object-cover border border-slate-200/50 shadow-sm"
          />
        </div>
      </div>

      {/* Main chat area */}
      <div className="w-3/4 flex flex-col bg-[radial-gradient(ellipse_at_top_right,_var(--tw-gradient-stops))] from-gray-700/90 via-gray-800/90 to-gray-900/90">
        <header className="bg-gradient-to-r from-gray-700/80 to-gray-800/80 border-b border-gray-600/20 p-4 flex items-center justify-between shadow-md backdrop-blur-sm">
          <div>
            <div className="text-xl font-bold text-white">
              {currentRoom ? `${currentRoom.chatRoom.name}` : "Select a Room"}
            </div>
            {currentRoom && (
              <p className="text-sm text-gray-300">Created by: {currentRoom.chatRoom.createdBy?.username || "Unknown"}</p>
            )}
          </div>
          {currentRoom?.role === "CREATOR" && (
            <button
              onClick={() => {
                setEditName(currentRoom.chatRoom.name);
                setEditImage(null);
                setShowEditModal(true);
              }}
              className="text-blue-300 hover:text-blue-200 font-medium text-sm border border-blue-400/30 hover:border-blue-300/30 px-3 py-1.5 rounded-lg transition-all duration-200 shadow-sm hover:shadow-md bg-white/5 backdrop-blur-sm"
            >
              Edit Room
            </button>
          )}
        </header>

        {currentRoom ? (
          <Chat currentRoom={currentRoom} />
        ) : (
          <div className="text-center p-4 text-gray-300">No active chat room selected.</div>
        )}
      </div>

      {showEditModal && (
        <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/30 backdrop-blur-sm">
          <div className="bg-gradient-to-br from-slate-50/95 to-blue-50/95 p-6 rounded-2xl shadow-xl w-full max-w-md backdrop-blur-sm">
            <h2 className="text-lg font-semibold mb-4 text-gray-800">Edit Chat Room</h2>
            <input
              type="text"
              value={editName}
              onChange={(e) => setEditName(e.target.value)}
              placeholder="New chat room name"
              className="w-full border border-slate-200/50 rounded-lg px-4 py-2 mb-3 focus:outline-none focus:ring-2 focus:ring-blue-500/30 bg-white/50"
            />
            <input
              type="file"
              accept="image/*"
              onChange={(e) => setEditImage(e.target.files[0])}
              className="w-full mb-4"
            />
            <div className="flex justify-end gap-2">
              <button onClick={() => setShowEditModal(false)} className="px-4 py-2 text-gray-600 hover:text-gray-800">Cancel</button>
              <button onClick={handleUpdateChatroom} className="bg-blue-500/90 hover:bg-blue-600/90 text-white px-4 py-2 rounded-lg shadow-sm hover:shadow-md transition-all duration-200">Save</button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}

function App() {
  return (
    <Router>
      <Routes>
        <Route path="/login" element={<Login />} />
        <Route path="/signup" element={<Signup />} />
        <Route
          path="/"
          element={
            <ProtectedRoute>
              <ChatRooms />
            </ProtectedRoute>
          }
        />
        <Route
          path="/create-room"
          element={
            <ProtectedRoute>
              <CreateRoom />
            </ProtectedRoute>
          }
        />
        <Route
          path="/join-room"
          element={
            <ProtectedRoute>
              <JoinRoom />
            </ProtectedRoute>
          }
        />
        <Route
          path="/profile"
          element={
            <ProtectedRoute>
              <EditProfile />
            </ProtectedRoute>
          }
        />
      </Routes>
    </Router>
  );
}

export default App;