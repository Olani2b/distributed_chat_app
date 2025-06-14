package com.dsmt.demo.demo1.service;

import com.dsmt.demo.demo1.entities.ChatRoom;
import com.dsmt.demo.demo1.entities.Message;
import com.dsmt.demo.demo1.entities.User;
import com.dsmt.demo.demo1.repository.MessageRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Map;

@Service
public class MessageService {

    @Autowired
    private MessageRepository messageRepository;
    private final WebClient webClient = WebClient.builder()
            .baseUrl("http://localhost:8081") // Add base URL
            .build();

    public void joinRoom(String room, String user) {
        webClient.post()
                .uri("/join") //  Match Erlang's `/join` endpoint
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .bodyValue(Map.of("room", room, "user", user))
                .retrieve()
                .bodyToMono(String.class)
                .subscribe();
    }
    public List<Message> getMessagesByChatRoomPaginated(Long chatRoomId, int page, int size) {
        Pageable pageable = PageRequest.of(page, size, Sort.by("timestamp").descending());
        return messageRepository.findByChatRoomId(chatRoomId, pageable);
    }
    public Message getLastMessageByChatRoom(Long chatRoomId) {
        return messageRepository
                .findTop1ByChatRoomIdOrderByTimestampDesc(chatRoomId)
                .orElse(null);
    }


    public void sendMessageToErlang(String room, String user, String message) {
        webClient.post()
                .uri("/broadcast") //  Match Erlang's `/broadcast` endpoint
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .bodyValue(Map.of("room", room, "user", message))
                .retrieve()
                .bodyToMono(String.class)
                .subscribe();
    }
    public Message createMessage(ChatRoom chatRoom, User user, String messageContent, OffsetDateTime timestamp ) {
        Message message = new Message();
        message.setChatRoom(chatRoom);
        message.setUser(user);
        message.setMessage(messageContent);
        if (timestamp != null) {
            message.setTimestamp(timestamp.toLocalDateTime());
        } else {
            message.setTimestamp(LocalDateTime.now());
        }

        return messageRepository.save(message);
    }

    public List<Message> getMessagesByChatRoom(Long chatRoomId) {
        return messageRepository.findByChatRoomId(chatRoomId);
    }

    public List<Message> getMessagesByUser(Long userId) {
        return messageRepository.findByUserId(userId);
    }

    public Message getMessageById(Long id) {
        return messageRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Message not found with ID: " + id));
    }

    public void deleteMessage(Long id) {
        if (!messageRepository.existsById(id)) {
            throw new RuntimeException("Message not found with ID: " + id);
        }
        messageRepository.deleteById(id);
    }
}
