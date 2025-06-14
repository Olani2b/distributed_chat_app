package com.dsmt.demo.demo1.dto;

import java.time.OffsetDateTime;

public class MessageRequestDTO {
    private Long userId;
    private Long chatRoomId;
    private String message;
    private OffsetDateTime timestamp;
    //  Add a no-argument constructor (required for JSON parsing)
    public MessageRequestDTO() {}

    //  Getters and Setters
    public Long getUserId() { return userId; }
    public void setUserId(Long userId) { this.userId = userId; }

    public Long getChatRoomId() { return chatRoomId; }
    public void setChatRoomId(Long chatRoomId) { this.chatRoomId = chatRoomId; }

    public String getMessage() { return message; }
    public void setMessage(String message) { this.message = message; }
    public OffsetDateTime getTimestamp() { return timestamp; }
    public void setTimestamp(OffsetDateTime timestamp) { this.timestamp = timestamp; }
}


