package com.dsmt.demo.demo1.service;

import com.dsmt.demo.demo1.entities.ChatRoom;
import com.dsmt.demo.demo1.entities.User;
import com.dsmt.demo.demo1.entities.UserChatroom;
import com.dsmt.demo.demo1.repository.ChatRoomRepository;
import com.dsmt.demo.demo1.repository.UserChatroomRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class UserChatroomService {
    @Autowired
    private UserChatroomRepository userChatroomRepository;
    @Autowired
    private ChatRoomRepository chatRoomRepository;

    public UserChatroom createUserChatroom(UserChatroom userChatroom){

      userChatroom.setCreatedAt(LocalDateTime.now());
      return userChatroomRepository.save(userChatroom);

    };

//    public List<UserChatroom> getUserChattroombyId(long ID) {
//        List<UserChatroom> chatrooms = userChatroomRepository.findByUserId(ID);
//        if (chatrooms.isEmpty()) {
//            throw new RuntimeException("User not found");
//        }
//        return chatrooms;
//    }
public List<UserChatroom> getUserChatroomsByUserId(long userId) {
    return userChatroomRepository.findByUserId(userId);
}

    public List<ChatRoom> getUnjoinedChatRooms(Long userId) {
        List<Long> joinedRoomIds = userChatroomRepository.findJoinedChatroomIdsByUserId(userId);

        // Get all chat rooms and filter out the joined ones
        return chatRoomRepository.findAll().stream()
                .filter(chatRoom -> !joinedRoomIds.contains(chatRoom.getId()))
                .collect(Collectors.toList());
    }


}
