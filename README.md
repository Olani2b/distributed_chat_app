# Distributed Real-Time Chat System

A distributed chat application built with React, Spring Boot, and Erlang. The system enables real-time messaging across chatrooms with fault tolerance, synchronization, and persistence.

## Features

- User authentication with JWT
- Create/join chatrooms with optional images
- Real-time messaging via WebSocket (Erlang Cowboy)
- Profile editing (username + avatar)
- Message history with reload support
- Fault-tolerant and eventually consistent design

## Tech Stack

- **Frontend:** React + Tailwind CSS
- **Backend:** Spring Boot (REST API + PostgreSQL)
- **Messaging Layer:** Erlang (Cowboy WebSocket, ETS)

## Architecture

React (Frontend SPA)  
↕ WebSocket  
Erlang (Messaging Node)  
↕ HTTP + API Key  
Spring Boot (Backend API)  
↕  
PostgreSQL (Database)

## How It Works

- React handles the user interface and WebSocket connection to Erlang.
- Erlang manages real-time messaging and temporarily stores messages using ETS.
- Erlang periodically syncs messages to the backend for persistence.
- Spring Boot handles authentication, chatroom/message APIs, and database storage.

## Setup Instructions

### 1. React Frontend

```bash
cd frontend
npm install
npm start
```

### 2. Spring Boot Backend

```bash
cd backend
# Configure PostgreSQL and JWT secret in application.properties
./mvnw spring-boot:run
```

### 3. Erlang Messaging Node

```bash
cd messaging-node
erl -pa ebin -s chat_server start
```

> Set API_KEY for Erlang as an environment variable before running.

## Author

Olani B. Gerba  
University of Pisa  
Distributed Systems and Middleware Technologies, 2024/2025
