// Storage utility for managing extension data
export class StorageManager {
  constructor() {
    this.cache = new Map();
  }

  async get(key) {
    if (this.cache.has(key)) {
      return this.cache.get(key);
    }
    
    const result = await chrome.storage.local.get(key);
    this.cache.set(key, result[key]);
    return result[key];
  }

  async set(key, value) {
    await chrome.storage.local.set({ [key]: value });
    this.cache.set(key, value);
  }

  async remove(key) {
    await chrome.storage.local.remove(key);
    this.cache.delete(key);
  }

  async clear() {
    await chrome.storage.local.clear();
    this.cache.clear();
  }
}