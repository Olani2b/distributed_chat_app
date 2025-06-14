package com.dsmt.demo.demo1.controller;

import com.dsmt.demo.demo1.entities.User;
import com.dsmt.demo.demo1.repository.UserRepository;
import com.dsmt.demo.demo1.JwtUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.MediaTypeFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

@RestController
@RequestMapping("/api")
public class AuthController {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private PasswordEncoder passwordEncoder;

//    @PostMapping("/signup")
//    public String signup(@RequestBody User user) {
//        user.setPassword(passwordEncoder.encode(user.getPassword()));
//        userRepository.save(user);
//        return "User registered successfully!";
//    }
@PostMapping("/signup")
public ResponseEntity<?> signup(
        @RequestParam("username") String username,
        @RequestParam("email") String email,
        @RequestParam("password") String password,
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

        User newUser = new User();
        newUser.setUsername(username);
        newUser.setEmail(email);
        newUser.setPassword(passwordEncoder.encode(password));
        newUser.setImage_url(imageFilename);

        userRepository.save(newUser);
        return ResponseEntity.ok("User registered successfully!");
    } catch (Exception e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Signup failed: " + e.getMessage());
    }
}
    @GetMapping("/profile-images/{filename}")
    public ResponseEntity<Resource> getImage(@PathVariable String filename) {
        try {
            Path file = Paths.get("uploads").resolve(filename);
            Resource resource = new UrlResource(file.toUri());
            if (resource.exists()) {
                return ResponseEntity.ok()
                        .contentType(MediaTypeFactory.getMediaType(resource).orElse(MediaType.APPLICATION_OCTET_STREAM))
                        .body(resource);
            } else {
                return ResponseEntity.notFound().build();
            }
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }



    @PostMapping("/login")
    public ResponseEntity<Map<String, Object>> login(@RequestBody User user) {
        Optional<User> foundUser = userRepository.findByUsername(user.getUsername());

        Map<String, Object> response = new HashMap<>();

        if (foundUser.isPresent() && passwordEncoder.matches(user.getPassword(), foundUser.get().getPassword())) {
            String token = JwtUtil.generateToken(foundUser.get().getId());
            response.put("userId", foundUser.get().getId());
            response.put("token", token);
            response.put("username", foundUser.get().getUsername());
            response.put("image_url", foundUser.get().getImage_url());
            return ResponseEntity.ok(response);
        }

        response.put("message", "Invalid username or password");
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(response);
    }



}
