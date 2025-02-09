// Feature statistics tracking utility
export class FeatureStats {
  constructor() {
    this.storage = chrome.storage.local;
  }

  async getAll() {
    try {
      const stats = await this.storage.get('featureStats') || this.getDefaultStats();
      return stats.featureStats;
    } catch (error) {
      console.error('Failed to get stats:', error);
      return this.getDefaultStats().featureStats;
    }
  }

  async increment(key, amount = 1) {
    try {
      const stats = await this.getAll();
      stats[key] = (stats[key] || 0) + amount;
      await this.storage.set({ featureStats: stats });
      return stats[key];
    } catch (error) {
      console.error(`Failed to increment ${key}:`, error);
    }
  }

  async set(key, value) {
    try {
      const stats = await this.getAll();
      stats[key] = value;
      await this.storage.set({ featureStats: stats });
      return value;
    } catch (error) {
      console.error(`Failed to set ${key}:`, error);
    }
  }

  async reset() {
    try {
      await this.storage.set({ featureStats: this.getDefaultStats().featureStats });
    } catch (error) {
      console.error('Failed to reset stats:', error);
    }
  }

  getDefaultStats() {
    return {
      featureStats: {
        columnCount: 0,
        blockedCount: 0,
        shadowbanStatus: 'Unknown',
        bookmarkCount: 0,
        scheduledCount: 0,
        filterCount: 0
      }
    };
  }
}