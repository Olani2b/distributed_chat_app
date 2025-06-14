package com.dsmt.demo.demo1.controller;

import com.dsmt.demo.demo1.entities.ChatRoom;
import com.dsmt.demo.demo1.service.ChatRoomService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.MediaType;
import org.springframework.http.MediaTypeFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

@RestController
@RequestMapping("/chatrooms")
public class ChatRoomController {
    @Autowired
    private ChatRoomService chatRoomService;

    // Create a new ChatRoom
    @PostMapping
    public ResponseEntity<ChatRoom> createChatRoom(
            @RequestParam("name") String name,
            @RequestParam(value = "image", required = false) MultipartFile image
    ) {
        ChatRoom created = chatRoomService.createChatRoomWithImage(name, image);
        return ResponseEntity.ok(created);
    }
    @GetMapping("/image/{filename}")
    public ResponseEntity<Resource> getChatRoomImage(@PathVariable String filename) {
        try {
            Path file = Paths.get("uploads/chatrooms").resolve(filename);
            Resource resource = new UrlResource(file.toUri());

            if (resource.exists()) {
                return ResponseEntity.ok()
                        .contentType(MediaTypeFactory.getMediaType(resource).orElse(MediaType.APPLICATION_OCTET_STREAM))
                        .body(resource);
            } else {
                return ResponseEntity.notFound().build();
            }
        } catch (Exception e) {
            return ResponseEntity.internalServerError().build();
        }
    }


    // Get all ChatRooms
    @GetMapping
    public ResponseEntity<List<ChatRoom>> getAllChatRooms() {
        return ResponseEntity.ok(chatRoomService.getAllChatRooms());
    }

    // Get a ChatRoom by ID
    @GetMapping("/{id}")
    public ResponseEntity<ChatRoom> getChatRoomById(@PathVariable Long id) {
        return ResponseEntity.ok(chatRoomService.getChatRoomById(id));
    }

    // Update a ChatRoom by ID
    @PutMapping(path = "/{id}", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<ChatRoom> updateChatRoom(
            @PathVariable Long id,
            @RequestParam("name") String name,
            @RequestParam(value = "image", required = false) MultipartFile image
    ){
        return ResponseEntity.ok(chatRoomService.updateChatRoomWithImage(id, name, image));

    }


    // Delete a ChatRoom by ID
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteChatRoom(@PathVariable Long id) {
        chatRoomService.deleteChatRoom(id);
        return ResponseEntity.noContent().build();
    }
}
