package com.dsmt.demo.demo1.repository;

import com.dsmt.demo.demo1.entities.ChatRoom;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ChatRoomRepository extends JpaRepository<ChatRoom, Long> {
}
