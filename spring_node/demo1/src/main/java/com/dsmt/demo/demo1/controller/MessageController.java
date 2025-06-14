package com.dsmt.demo.demo1.controller;

import com.dsmt.demo.demo1.dto.MessageRequestDTO;
import com.dsmt.demo.demo1.entities.ChatRoom;
import com.dsmt.demo.demo1.entities.Message;
import com.dsmt.demo.demo1.entities.User;
import com.dsmt.demo.demo1.service.ChatRoomService;
import com.dsmt.demo.demo1.service.MessageService;
import com.dsmt.demo.demo1.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/messages")
public class MessageController {

    @Autowired
    private MessageService messageService;

    @Autowired
    private ChatRoomService chatRoomService;

    @Autowired
    private UserService userService;

    @PostMapping("/{chatRoomId}/join/{userId}")
    public ResponseEntity<?> joinChatRoom(
            @PathVariable String chatRoomId,
            @PathVariable String userId) {

        messageService.joinRoom(chatRoomId, userId);

        return ResponseEntity.ok(Map.of("status", "User Joined"));
    }

    //  Fix: Changed @RequestParam to @PathVariable for consistency
    @PostMapping("/save")
    public ResponseEntity<?> saveMessages(
            @RequestBody List<MessageRequestDTO> messages,
            @RequestHeader(value = "X-API-Key", required = false) String apiKey) {
        System.out.println("Received API key: " + apiKey);
        System.out.println("Expected API key: " + System.getenv("API_KEY"));

        // Validate API key
        String expectedApiKey = System.getenv("API_KEY"); // or use @Value from properties
        if (expectedApiKey == null || apiKey == null || !expectedApiKey.trim().equals(apiKey.trim())) {
            return ResponseEntity.status(401).body(Map.of("error", "Unauthorized: Invalid API Key"));
        }


        List<Message> savedMessages = new ArrayList<>();
        for (MessageRequestDTO msg : messages) {
            ChatRoom chatRoom = chatRoomService.getChatRoomById(msg.getChatRoomId());
            User user = userService.getUserById(msg.getUserId());

            Message message = messageService.createMessage(
                    chatRoom,
                    user,
                    msg.getMessage(),
                    msg.getTimestamp()
            );

            savedMessages.add(message);
        }

        return ResponseEntity.ok(savedMessages);
    }





    @GetMapping("/chatroom/{chatRoomId}")
    public ResponseEntity<List<Message>> getMessagesByChatRoom(@PathVariable Long chatRoomId) {
        return ResponseEntity.ok(messageService.getMessagesByChatRoom(chatRoomId));
    }

    @GetMapping("/user/{userId}")
    public ResponseEntity<List<Message>> getMessagesByUser(@PathVariable Long userId) {
        return ResponseEntity.ok(messageService.getMessagesByUser(userId));
    }

    @GetMapping("/{id}")
    public ResponseEntity<Message> getMessageById(@PathVariable Long id) {
        return ResponseEntity.ok(messageService.getMessageById(id));
    }
    @GetMapping("/chatroom/{chatRoomId}/last")
    public ResponseEntity<Message> getLastMessage(@PathVariable Long chatRoomId) {
        return ResponseEntity.ok(messageService.getLastMessageByChatRoom(chatRoomId));
    }

    @GetMapping("/chatroom/{chatRoomId}/history")
    public ResponseEntity<List<Message>> getMessagesHistory(
            @PathVariable Long chatRoomId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        return ResponseEntity.ok(messageService.getMessagesByChatRoomPaginated(chatRoomId, page, size));
    }


    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteMessage(@PathVariable Long id) {
        messageService.deleteMessage(id);
        return ResponseEntity.noContent().build();
    }
}
