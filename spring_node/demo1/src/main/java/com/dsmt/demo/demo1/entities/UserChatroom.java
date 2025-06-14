package com.dsmt.demo.demo1.entities;

import jakarta.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "user_chatroom")
public class UserChatroom {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "user_id", nullable = false)
    private Long userId;

    @Column(name = "chatroom_id", nullable = false)
    private Long chatroomId;

    @Column(name = "role", nullable = true)
    private String role; // New role column

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "chatroom_id", insertable = false, updatable = false)
    private ChatRoom chatRoom;

    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
    }

    public UserChatroom() {}

    public UserChatroom(Long userId, Long chatroomId, String role) {
        this.userId = userId;
        this.chatroomId = chatroomId;
        this.role = role;
    }

    // Getters and Setters
    public Long getId() {
        return id;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getChatroomId() {
        return chatroomId;
    }

    public void setChatroomId(Long chatroomId) {
        this.chatroomId = chatroomId;
    }

    public String getRole() {
        return role;
    }

    public void setRole(String role) {
        this.role = role;
    }

    public ChatRoom getChatRoom() {
        return chatRoom;
    }

    public void setChatRoom(ChatRoom chatRoom) {
        this.chatRoom = chatRoom;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    @Override
    public String toString() {
        return "UserChatroom{" +
                "id=" + id +
                ", userId=" + userId +
                ", chatroomId=" + chatroomId +
                ", role='" + role + '\'' +
                ", chatRoom=" + (chatRoom != null ? chatRoom.getName() : "null") +
                ", createdAt=" + createdAt +
                '}';
    }
}
