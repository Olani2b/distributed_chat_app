package com.dsmt.demo.demo1.repository;

import com.dsmt.demo.demo1.entities.User;
import com.dsmt.demo.demo1.entities.UserChatroom;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface UserChatroomRepository extends JpaRepository<UserChatroom, Long> {
    List<UserChatroom> findByUserId(Long userId);

    // Get IDs of chat rooms that the user has already joined
    @Query("SELECT uc.chatRoom.id FROM UserChatroom uc WHERE uc.userId = :userId")
    List<Long> findJoinedChatroomIdsByUserId(Long userId);
}

