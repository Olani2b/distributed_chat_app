package com.dsmt.demo.demo1.service;

import com.dsmt.demo.demo1.entities.ChatRoom;
import com.dsmt.demo.demo1.entities.User;
import com.dsmt.demo.demo1.entities.UserChatroom;
import com.dsmt.demo.demo1.repository.ChatRoomRepository;
import com.dsmt.demo.demo1.repository.UserChatroomRepository;
import com.dsmt.demo.demo1.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import java.nio.file.*;
@Service
public class ChatRoomService {
    @Autowired
    private ChatRoomRepository chatRoomRepository;
    @Autowired
    private UserChatroomRepository userChatroomRepository;
    @Autowired
    private UserRepository userRepository;

    public ChatRoom createChatRoomWithImage(String name, MultipartFile image) {
        String imageFilename = null;

        if (image != null && !image.isEmpty()) {
            try {
                Path uploadPath = Paths.get("uploads/chatrooms");
                Files.createDirectories(uploadPath);

                String extension = image.getOriginalFilename().substring(image.getOriginalFilename().lastIndexOf('.'));
                imageFilename = UUID.randomUUID() + extension;

                Path destination = uploadPath.resolve(imageFilename);
                Files.copy(image.getInputStream(), destination, StandardCopyOption.REPLACE_EXISTING);
            } catch (IOException e) {
                throw new RuntimeException("Failed to store chatroom image", e);
            }
        }

        ChatRoom chatRoom = new ChatRoom();
        chatRoom.setName(name);
        chatRoom.setCreatedAt(LocalDateTime.now());
        chatRoom.setImageUrl(imageFilename);

        ChatRoom saved = chatRoomRepository.save(chatRoom);

        // Add creator to the chatroom
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();

        User user = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("User not found"));
        chatRoom.setCreatedBy(user);
        //UserChatroom userChatroom = new UserChatroom(user.getId(), saved.getId());
        UserChatroom userChatroom = new UserChatroom(user.getId(), saved.getId(), "CREATOR");

        userChatroom.setCreatedAt(LocalDateTime.now());
         // set the creator

        userChatroomRepository.save(userChatroom);

        return saved;
    }
    // Create a new ChatRoom
    public ChatRoom createChatRoom(ChatRoom chatRoom) {
        chatRoom.setCreatedAt(LocalDateTime.now());
        ChatRoom savedChatRoom = chatRoomRepository.save(chatRoom);

        // Get the current logged-in username
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String username = authentication.getName();

        // Find the user
        User user = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("User not found"));

        // Create a UserChatroom association
        UserChatroom userChatroom = new UserChatroom(user.getId(), savedChatRoom.getId(), "CREATOR");
        userChatroom.setCreatedAt(LocalDateTime.now());
        userChatroomRepository.save(userChatroom);

        return savedChatRoom;
    }

    // Retrieve all ChatRooms
    public List<ChatRoom> getAllChatRooms() {
        return chatRoomRepository.findAll();
    }

    // Retrieve a ChatRoom by ID
    public ChatRoom getChatRoomById(Long id) {
        return chatRoomRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("ChatRoom not found with ID: " + id));
    }

    // Update a ChatRoom by ID
    public ChatRoom updateChatRoom(Long id, ChatRoom updatedChatRoom) {
        ChatRoom chatRoom = getChatRoomById(id); // Fetch existing ChatRoom
        chatRoom.setName(updatedChatRoom.getName());
        return chatRoomRepository.save(chatRoom);
    }
    public ChatRoom updateChatRoomWithImage(Long id, String name, MultipartFile image) {
        ChatRoom chatRoom = getChatRoomById(id); // reuse existing logic

        chatRoom.setName(name);

        if (image != null && !image.isEmpty()) {
            try {
                Path uploadPath = Paths.get("uploads/chatrooms");
                Files.createDirectories(uploadPath);

                String extension = image.getOriginalFilename()
                        .substring(image.getOriginalFilename().lastIndexOf('.'));
                String imageFilename = UUID.randomUUID() + extension;

                Path destination = uploadPath.resolve(imageFilename);
                Files.copy(image.getInputStream(), destination, StandardCopyOption.REPLACE_EXISTING);

                chatRoom.setImageUrl(imageFilename);
            } catch (IOException e) {
                throw new RuntimeException("Failed to update chatroom image", e);
            }
        }

        return chatRoomRepository.save(chatRoom);
    }

    // Delete a ChatRoom by ID
    public void deleteChatRoom(Long id) {
        if (!chatRoomRepository.existsById(id)) {
            throw new RuntimeException("ChatRoom not found with ID: " + id);
        }
        chatRoomRepository.deleteById(id);
    }
}
