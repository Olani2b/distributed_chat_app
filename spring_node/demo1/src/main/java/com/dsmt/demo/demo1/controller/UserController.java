package com.dsmt.demo.demo1.controller;

import com.dsmt.demo.demo1.entities.User;
import com.dsmt.demo.demo1.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/users")
public class UserController {
    @Autowired
    private UserService userService;

    @PostMapping
    public ResponseEntity<User> createUser(@RequestBody User user) {
        return ResponseEntity.ok(userService.createUser(user));
    }

    @GetMapping
    public ResponseEntity<List<User>> getAllUsers() {
        return ResponseEntity.ok(userService.getAllUsers());
    }

    @GetMapping("/{id}")
    public ResponseEntity<User> getUserById(@PathVariable Long id) {
        return ResponseEntity.ok(userService.getUserById(id));
    }

    @PutMapping(value = "/{id}", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<?> updateUser(
            @PathVariable Long id,
            @RequestParam("username") String username,
            @RequestParam(value = "image", required = false) MultipartFile image
    ) {
        try {
            String imageFilename = null;

            if (image != null && !image.isEmpty()) {
                String extension = image.getOriginalFilename().substring(image.getOriginalFilename().lastIndexOf("."));
                imageFilename = UUID.randomUUID() + extension;

                Path uploadDir = Paths.get("uploads");
                Files.createDirectories(uploadDir);
                Files.copy(image.getInputStream(), uploadDir.resolve(imageFilename), StandardCopyOption.REPLACE_EXISTING);
            }

            User user = userService.getUserById(id); // or use repo directly if public
            user.setUsername(username);
            user.setImage_url(imageFilename != null ? imageFilename : user.getImage_url());

            userService.saveUser(user); // create this method or expose repo.save(user)

            return ResponseEntity.ok(user);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Update failed: " + e.getMessage());
        }
    }



    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteUser(@PathVariable Long id) {
        userService.deleteUser(id);
        return ResponseEntity.noContent().build();
    }
}
