import React, { useState, useEffect, useRef } from "react";

function Chat({ currentRoom }) {
  const [message, setMessage] = useState("");
  const [messages, setMessages] = useState({});
  const chatEndRef = useRef(null);

  // Auto-scroll to latest message
  useEffect(() => {
    chatEndRef.current?.scrollIntoView({ behavior: "smooth" });
  }, [messages]);

  const sendMessage = () => {
    if (message.trim() === "") return;
    setMessages((prev) => ({
      ...prev,
      [currentRoom]: [...(prev[currentRoom] || []), message],
    }));
    setMessage("");
  };
  const handleKeyDown = (e) => {
    if (e.key === 'Enter') {
      sendMessage();
    }
  };

  return (
    <div className="flex-1 flex flex-col justify-between bg-gray-900">
      {/* Chat Messages */}
      <div className="flex-1 overflow-y-auto p-4 space-y-2">
        {(messages[currentRoom] || []).map((msg, index) => (
          <div key={index} className="bg-gray-700 p-3 rounded-md">
            {msg}
          </div>
        ))}
        <div ref={chatEndRef}></div>
      </div>

      {/* Message Input */}
      <div className="flex p-4 bg-gray-800">
        <input
          type="text"
          className="flex-1 p-3 rounded-l-md text-black"
          placeholder="Type a message..."
          value={message}
          onChange={(e) => setMessage(e.target.value)}
          onKeyDown={handleKeyDown}
        />
        <button onClick={sendMessage} className="bg-blue-500 px-6 py-3 rounded-r-md">
          Send
        </button>
      </div>
    </div>
  );
}

export default Chat;
