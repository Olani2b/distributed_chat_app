package com.dsmt.demo.demo1.controller;

import com.dsmt.demo.demo1.entities.ChatRoom;
import com.dsmt.demo.demo1.entities.Message;
import com.dsmt.demo.demo1.entities.User;
import com.dsmt.demo.demo1.entities.UserChatroom;
import com.dsmt.demo.demo1.service.UserChatroomService;
import com.dsmt.demo.demo1.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/userchatroom")
public class UserChatroomController {
    @Autowired
    private UserChatroomService userChatroomService;

    @PostMapping
    public ResponseEntity<UserChatroom> createUser(@RequestBody UserChatroom userChatroom) {
        return ResponseEntity.ok(userChatroomService.createUserChatroom(userChatroom));
    }


//    @GetMapping("/{id}")
//    public ResponseEntity<List<UserChatroom>> getUserById(@PathVariable Long id) {
//        return ResponseEntity.ok(userChatroomService.getUserChattroombyId(id));
//    }
@GetMapping("/{userId}")
public ResponseEntity<List<UserChatroom>> getUserChatroomsByUserId(@PathVariable Long userId) {
    List<UserChatroom> chatrooms = userChatroomService.getUserChatroomsByUserId(userId);
    return ResponseEntity.ok(chatrooms);
}

    @GetMapping("/unjoined/{userId}")
    public ResponseEntity<List<ChatRoom>> getUnjoinedChatRooms(@PathVariable Long userId) {
        List<ChatRoom> unjoinedRooms = userChatroomService.getUnjoinedChatRooms(userId);
        return ResponseEntity.ok(unjoinedRooms);
    }



}
