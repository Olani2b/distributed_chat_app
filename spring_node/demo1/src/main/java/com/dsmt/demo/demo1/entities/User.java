package com.dsmt.demo.demo1.entities;

import jakarta.persistence.*;

import java.time.LocalDateTime;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;


@JsonIgnoreProperties({"hibernateLazyInitializer", "handler"})
@Entity
@Table(name = "users")
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String username;
    private LocalDateTime createdAt;
    private String email;
    private String password;
    private String image_url;

    public User(Long id, String username, LocalDateTime createdAt, String email, String password, String image_url) {
        this.id = id;
        this.username = username;
        this.createdAt = createdAt;
        this.email = email;
        this.password = password;
        this.image_url = image_url;
    }

    public User() {
    }

    public Long getId() {
        return id;
    }
    public String getEmail() {return email;}
    public String getImage_url(){
        return  image_url;
    }
    public String getPassword(){return password;};
    public String getUsername() {
        return username;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setEmail(String email) {

        this.email = email;
    }
    public void setPassword(String password){
        this.password = password;
    }
    public void setId(Long id) {
        this.id = id;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }
    public void setImage_url(String image_url){
        this.image_url = image_url;
    }
}
