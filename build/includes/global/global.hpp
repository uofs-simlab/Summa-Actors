#pragma once

#include <chrono>
#include <optional>
#include <iostream>
#include <bits/stdc++.h>
#include <unistd.h>
#include "json.hpp"


using json = nlohmann::json;

extern bool debug;


/**
 * Return the time between to time points
 */
double calculateTime(std::chrono::time_point<std::chrono::system_clock> start, 
    std::chrono::time_point<std::chrono::system_clock> end);

template <typename T>
std::optional<T> getSettings(std::string settings_file_path, std::string key_1, std::string key_2, 
    T return_value) {
    json settings;
    std::string summa_actors_settings = "/Summa_Actors_Settings.json";
    std::ifstream settings_file(settings_file_path + summa_actors_settings);
    settings_file >> settings;
    settings_file.close();
    
    // find first key
    try {
        if (settings.find(key_1) != settings.end()) {
            json key_1_settings = settings[key_1];

            // find value behind second key
            if (key_1_settings.find(key_2) != key_1_settings.end()) {
                return key_1_settings[key_2];
            } else 
                return {};

        } else {
            return {}; // return none in the optional (error value)
        }
    } catch (json::exception& e) {
        std::cout << e.what() << "\n";
        return {};
    }
   
}