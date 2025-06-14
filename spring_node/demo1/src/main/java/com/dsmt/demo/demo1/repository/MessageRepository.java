package com.dsmt.demo.demo1.repository;

import com.dsmt.demo.demo1.entities.Message;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface MessageRepository extends JpaRepository<Message, Long> {
    List<Message> findByChatRoomId(Long chatRoomId);

    List<Message> findByUserId(Long userId);
    List<Message> findByChatRoomId(Long chatRoomId, Pageable pageable);
    Optional<Message> findTop1ByChatRoomIdOrderByTimestampDesc(Long chatRoomId);


}
