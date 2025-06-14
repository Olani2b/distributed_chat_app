package com.dsmt.demo.demo1.controller;

import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

@RestController
@RequestMapping("/api/images")
public class ImageController {

    @GetMapping("/{filename:.+}")
    public ResponseEntity<Resource> getUserImage(@PathVariable String filename) {
        try {
            Path imageStoragePath = Paths.get("uploads").toAbsolutePath().normalize();
            Path filePath = imageStoragePath.resolve(filename).normalize(); // Normalize to prevent path traversal

            System.out.println("Resolved image path: " + filePath); // for debugging

            Resource resource = new UrlResource(filePath.toUri());

            if (!resource.exists() || !resource.isReadable()) {
                System.out.println("File not found or not readable");
                return ResponseEntity.status(403).build();
            }

            String contentType = Files.probeContentType(filePath);
            return ResponseEntity.ok()
                    .contentType(MediaType.parseMediaType(contentType != null ? contentType : "application/octet-stream"))
                    .body(resource);

        } catch (Exception e) {
            e.printStackTrace(); //
            return ResponseEntity.internalServerError().build();
        }
    }

}
